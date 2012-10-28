(ns simple_shift_reduce_parsing.core
  (:use simple_shift_reduce_parsing.util
        simple_shift_reduce_parsing.feature
        simple_shift_reduce_parsing.evaluation
        simple_shift_reduce_parsing.topological_sort)
  (:use [clj-utils.io :only (serialize deserialize)])
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
   DEP     NP-SBJ  ROOT    NP-OBJ  DEP
   2       3       0       3       3

   the     luxury  auto    maker   last    year    sold    1,214   cars    in      the     u.s.
   DT      NN      NN      NN      JJ      NN      VBD     CD      NNS     IN      DT      NNP
   DEP     DEP     DEP     NP-SBJ  DEP     NP      ROOT    DEP     NP-OBJ  PP      DEP     NP
   4       4       4       7       6       7       0       9       7       7       12      10"
  (->> (split (slurp filename) #"\n\n")
       (map (fn [lines]
	      (let [[words pos-tags labels heads]
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

(defn- get-cli-opts [args]
  (cli/cli args
           ["--mode"]
	   ["--training-filename" "File name of training" :default "./data/train.lab"]
           ["--test-filename" "File name of test" :default "./data/test.lab"]
           ["--model-filename" "File name of the (saved|load) model" :default "parsing.model"]
	   ["--max-iter" "Number of maximum iterations" :default 10 :parse-fn #(Integer. %)]
           ["--eta" "Fobos hyper-parameter for update step" :default 1.0 :parse-fn #(Double. %)]
           ["--lambda" "Fobos hyper-parameter for regularization" :default 0.005 :parse-fn #(Double. %)]))

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
