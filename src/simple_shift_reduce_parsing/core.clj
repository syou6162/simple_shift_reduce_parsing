(ns simple_shift_reduce_parsing.core
  (:use simple_shift_reduce_parsing.util
        simple_shift_reduce_parsing.feature
	simple_shift_reduce_parsing.word
        simple_shift_reduce_parsing.topological_sort)
  (:use [fobos-multiclass-clj.util])
  (:use [fobos-multiclass-clj.multiclass
         :only (multiclass-examples argmax-label get-models get-label-scores)])
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:require [simple_shift_reduce_parsing.action :as action])
  (:gen-class))

(use '[clojure.string :only (split)])
(require '[clojure.tools.cli :as cli])
(import '(java.io File FileReader))
(import '[simple_shift_reduce_parsing.configuration Configuration])

(defn extract-surfaces [sentence]
  (if (not (vector? sentence))
    (if (empty? (:modifiers sentence))
      [(:surface sentence)]
      (apply conj [(:surface sentence)] (map extract-surfaces (:modifiers sentence))))
    (vec (map extract-surfaces sentence))))

(defn extract-heads' [sentence result]
  (if (not (vector? sentence))
    (if (empty? (:modifiers sentence))
      [{:head (:head sentence) :idx (:idx sentence)}]
      (vec (apply concat (conj result {:head (:head sentence) :idx (:idx sentence)})
                  (map #(extract-heads' % result) (:modifiers sentence)))))
    (vec (map #(extract-heads' % result) sentence))))

(defn extract-heads [sentence]
  (map :head (sort-by :idx (first (extract-heads' sentence [])))))

(def Root :root)

(defn read-mst-format-file [filename]
  "Read correct parsed sentences from mst formal file.
   File format is as follows:

   ms.     haag    plays   elianti .
   NNP     NNP     VBZ     NNP     .
   2       3       0       3       3

   he      has     n't     been    able    to      replace the     m'bow   cabal   .
   PRP     VBZ     RB      VBN     JJ      TO      VB      DT      NNP     NN      .
   2       0       2       2       4       7       5       10      10      7       2"
  (->> (split (slurp filename) #"\n\n")
       (map (fn [lines]
	      (let [[words pos-tags heads]
		    (map (fn [line]
			   (map clojure.string/lower-case (split line #"\t")))
                         (split lines #"\n"))]
		(vec (map (fn [w pos-tag idx head]
			    (struct word w pos-tag
				    idx head))
			  (vec (cons Root words))
			  (vec (cons Root pos-tags))
			  (vec (range (inc (count words))))
			  (vec (cons -1 (map
                                         #(Integer/parseInt %)
                                         heads))))))))
       (vec)))

(defn parse [models sentence]
  (loop [config (config/make-Configuration sentence)]
    (if (empty? (:input config))
      (let [pairs (-> config :relations :modifier-to-head)]
        (reduce (fn [sent [modifier head]]
                  (assoc-in sent [(:idx modifier) :head] (:idx head)))
                sentence
                pairs))
      (let [action (argmax-label models (get-fv config))]
        (recur ((action/action-mapping action) config))))))

(defn get-dependency-accuracy [original-sentences parsed-sentences]
  "original-sentences: flat sentences
   parsed-sentences: sentences with hierarchical structure"
  (let [original-sentences (rest original-sentences) ;; remove root
        parsed-sentences (rest parsed-sentences) ;; remove root
        pairs (map (fn [[original-sentence parsed-sentence]]
                     (let [num-of-correct-heads (reduce + (map (fn [[gold predict]]
                                                                 (if (= gold predict) 1.0 0.0))
                                                               (map vector
                                                                    (map :head original-sentence)
                                                                    (map :head parsed-sentence))))]
                       (vector num-of-correct-heads (count original-sentence))))
                   (map vector original-sentences parsed-sentences))
        corrects (map first pairs)
        lengths (map second pairs)]
    (/ (reduce + corrects) (reduce + lengths))))

(defn- get-cli-opts [args]
  (cli/cli args
           ["--mode"]
	   ["--training-filename" "File name of training" :default "./data/train.ulab"]
           ["--test-filename" "File name of test" :default "./data/test.ulab"]
           ["--model-filename" "File name of the (saved|load) model" :default "parsing.model"]
	   ["--max-iter" "Number of maximum iterations" :default 10 :parse-fn #(Integer. %)]
           ["--eta" "Fobos hyper-parameter for update step" :default 1.0 :parse-fn #(Double. %)]
           ["--lambda" "Fobos hyper-parameter for regularization" :default 0.005 :parse-fn #(Double. %)]))

(defn serialize [o filename]
  (with-open [outp (-> (File. filename) java.io.FileOutputStream. java.io.ObjectOutputStream.)]
    (.writeObject outp o)))

(defn deserialize [filename]
  (with-open [inp (-> (File. filename) java.io.FileInputStream. java.io.ObjectInputStream.)]
    (.readObject inp)))

(defn train-models [filename max-iter eta lambda model-filename]
  (let [training-examples (multiclass-examples
                           (for [sentence (read-mst-format-file filename)
                                 gold (generate-gold sentence)]
                             gold))
        _ (println "Started training...")
        models (get-models training-examples max-iter eta lambda)
        _ (println "Finished training...")]
    (serialize (reduce (fn [result [class model]]
                         (assoc result class
                                (assoc model :examples [])))
                       {}
                       models)
               model-filename)))

(defn load-models [model-filename]
  (deserialize model-filename))

(defn initialize-head-words [sentences]
  "Convert :head into nil in each words in the sentences.

   Ex)

   [{:surface :root, :pos-tag :root, :idx 0, :head -1}
    {:surface \"ms.\", :pos-tag \"NNP\", :idx 1, :head 2}]
   =>
   [{:surface :root, :pos-tag :root, :idx 0, :head nil}
    {:surface \"ms.\", :pos-tag \"NNP\", :idx 1, :head nil}]"
  (reduce
   (fn [result sent]
     (do
       (conj result (vec (map #(assoc % :head nil) sent)))))
   []
   sentences))

(defn evaluate-sentences [model-filename test-filename]
  (let [original-sentences (read-mst-format-file test-filename)
        _ (println (str "Finished reading " (count original-sentences) " instances from " test-filename "..."))
        models (load-models model-filename)
        _ (println "Finished loading models...")
        parsed-sentences (time (doall (map (partial parse models) (initialize-head-words original-sentences))))
        _ (println (str "Finished parsing " (count parsed-sentences) " sentences..."))]
    (do
      (println (get-dependency-accuracy original-sentences parsed-sentences))
      (dorun (map (fn [[idx sent]] (save-dependency-tree sent (str idx ".pdf")))
                  (map-indexed #(vector %1 %2) (take 100 parsed-sentences)))))))

(defn -main [& args]
  (let [[options args banner] (get-cli-opts args)]
    (cond (= "train" (:mode options)) (train-models (:training-filename options) (:max-iter options)
                                                    (:eta options) (:lambda options)
                                                    (:model-filename options))
          (= "test" (:mode options)) (evaluate-sentences (:model-filename options)
                                                         (:test-filename options))
          :else nil)))
