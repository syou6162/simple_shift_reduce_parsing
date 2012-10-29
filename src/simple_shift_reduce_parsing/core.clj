(ns simple_shift_reduce_parsing.core
  (:use simple_shift_reduce_parsing.util
        simple_shift_reduce_parsing.feature
        simple_shift_reduce_parsing.parse
        simple_shift_reduce_parsing.evaluation
        simple_shift_reduce_parsing.topological_sort)
  (:use [fobos-multiclass-clj.util])
  (:use [fobos-multiclass-clj.multiclass
         :only (multiclass-examples argmax-label get-models get-label-scores)])
  (:gen-class))

(require '[clojure.tools.cli :as cli])

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

(defn- get-cli-opts [args]
  (cli/cli args
           ["-h" "--help" "Show help" :default false :flag true]
           ["--mode" "(train|test|eval)"]
	   ["--training-filename" "File name of training" :default "./data/train.lab"]
           ["--test-filename" "File name of test" :default "./data/test.lab"]
           ["--model-filename" "File name of the (saved|load) model" :default "parsing.model"]
	   ["--max-iter" "Number of maximum iterations" :default 10 :parse-fn #(Integer. %)]
           ["--eta" "Fobos hyper-parameter for update step" :default 1.0 :parse-fn #(Double. %)]
           ["--lambda" "Fobos hyper-parameter for regularization" :default 0.005 :parse-fn #(Double. %)]))

(defn train-models [filename max-iter eta lambda model-filename]
  (let [training-examples (multiclass-examples
                           (for [sentence (take 10000 (read-mst-format-file filename))
                                 gold (generate-gold sentence)]
                             gold))
        _ (println "Started training...")
        models (get-models training-examples max-iter eta lambda)
        _ (println "Finished training...")]
    #_(serialize (reduce (fn [result [class model]]
                         (assoc result class
                                (assoc model :examples [])))
                       {}
                       models)
               model-filename)))

(defn test-mode [model-filename test-filename]
  (let [original-sentences (read-mst-format-file test-filename)
        models (load-models model-filename)
        parsed-sentences (map (partial parse models) (initialize-head-words original-sentences))]
    (print-mst-format-file parsed-sentences)))

(defn evaluate-sentences [model-filename test-filename]
  (let [original-sentences (read-mst-format-file test-filename)
        _ (println (str "Finished reading " (count original-sentences) " instances from " test-filename "..."))
        models (load-models model-filename)
        _ (println "Finished loading models...")
        parsed-sentences (time (doall (map (partial parse models) (initialize-head-words original-sentences))))
        _ (println (str "Finished parsing " (count parsed-sentences) " sentences..."))]
    (do
      (dorun (map (fn [[idx sent]] (save-dependency-tree sent (str idx ".pdf")))
                  (map-indexed #(vector %1 %2) (take 100 parsed-sentences))))
      (println (get-dependency-accuracy original-sentences parsed-sentences)))))

(defn export-mode [test-filename]
  (let [sentences (read-mst-format-file test-filename)]
    (dorun (map (fn [[idx sent]] (save-dependency-tree sent (str idx ".pdf")))
                (map-indexed #(vector %1 %2) sentences)))))

(defn -main [& args]
  (let [[options args banner] (get-cli-opts args)]
    (when (:help options)
      (println banner)
      (System/exit 0))
    (cond (= "train" (:mode options)) (train-models (:training-filename options) (:max-iter options)
                                                    (:eta options) (:lambda options)
                                                    (:model-filename options))
          (= "test" (:mode options)) (test-mode (:model-filename options)
                                                (:test-filename options))
          (= "eval" (:mode options)) (evaluate-sentences (:model-filename options)
                                                         (:test-filename options))
          (= "export" (:mode options)) (export-mode (:test-filename options))
          :else nil)
    (shutdown-agents)))
