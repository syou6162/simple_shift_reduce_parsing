(ns simple_shift_reduce_parsing.core
  (:use simple_shift_reduce_parsing.util
        simple_shift_reduce_parsing.feature
        simple_shift_reduce_parsing.parse
        simple_shift_reduce_parsing.evaluation)
  (:use [clj-utils.random :only (shuffle-with-random)])
  (:use [clj-utils.evaluation :only (get-accuracy)])
  (:import [de.bwaldvogel.liblinear Parameter])
  (:import [de.bwaldvogel.liblinear SolverType])
  (:use [liblinear.core
         :only (make-SVM classify do-cross-validation save-model load-model)])
  (:gen-class))

(import java.text.SimpleDateFormat)
(import (org.apache.log4j PatternLayout FileAppender))
(use 'clojure.tools.logging 'clj-logging-config.log4j)
(require '[clojure.tools.cli :as cli])

(defn- get-cli-opts [args]
  (cli/cli args
           ["-h" "--help" "Show help" :default false :flag true]
           ["--mode" "(train|test|eval|export)"]
           ["--k" "Number of cross-validations" :default 5 :parse-fn #(Integer. %)]
	   ["--training-filename" "File name of training" :default "train.txt"]
           ["--test-filename" "File name of test" :default "test.txt"]
           ["--model-filename" "File name of the (saved|load) model" :default "parsing.model"]
           ["--logging-level" "level of logging" :default :debug :parse-fn #(keyword %)]
           ["--feature-to-id-filename" "File name of the feature2id mapping" :default "feature-to-id.bin"]))

(defn train-model [{training-filename :training-filename
                    model-filename :model-filename
                    feature-to-id-filename :feature-to-id-filename
                    k :k}]
  (let [sentences (->> training-filename
                       (read-mst-format-file)
                       (shuffle-with-random))
        training-examples (for [sentence sentences
                                gold (generate-gold sentence)]
                            gold)
        gold-labels (mapv first training-examples)
        params (for [x [10 1 0.1 0.01 0.001], y [7.5 5 2.5 1]]
                 (new Parameter SolverType/L2R_LR_DUAL (* x y) 0.1))
        pairs (->> params
                   (shuffle-with-random)
                   (pmap
                    (fn [param]
                      (let [target (->> (do-cross-validation
                                         param training-examples k)
                                        (mapv int))
                            accuracy (get-accuracy gold-labels target)]
                        (info (str (. param getC) ": " accuracy))
                        [param accuracy])))
                   (vec))
        best-param (->> pairs
                        (sort-by second >)
                        (first)
                        (first))]
    (info (str "Number of training sentences: " (count sentences)))
    (info (str "Number of training examples: " (count training-examples)))
    (save-feature-to-id feature-to-id-filename)
    (clear-feature-to-id!)
    (info (str "Best param is C = " (. best-param getC)))
    (-> (make-SVM best-param training-examples)
        (save-model model-filename))))

(defn test-mode [{model-filename :model-filename
                  test-filename :test-filename
                  feature-to-id-filename :feature-to-id-filename}]
  (load-feature-to-id feature-to-id-filename)
  (let [original-sentences (read-mst-format-file test-filename)
        models (load-models model-filename)
        parsed-sentences (map (partial parse models) original-sentences)]
    (print-mst-format-file parsed-sentences)))

(defn evaluate-sentences [{model-filename :model-filename
                           test-filename :test-filename
                           feature-to-id-filename :feature-to-id-filename}]
  (load-feature-to-id feature-to-id-filename)
  (let [original-sentences (read-mst-format-file test-filename)
        _ (debug (str "Finished reading " (count original-sentences) " instances from " test-filename "..."))
        models (load-model model-filename)
        parsed-sentences (->> original-sentences
                              (pmap (partial parse models))
                              (doall))]
    (debug "Finished loading models...")
    (debug (str "Started parsing " (count original-sentences) " sentences..."))
    (debug (str "Finished parsing " (count parsed-sentences) " sentences..."))
    (info (str "Dependency accuracy: " (get-dependency-accuracy original-sentences parsed-sentences)))
    (info (str "Complete accuracy: " (get-complete-accuracy original-sentences parsed-sentences)))))

(defn export-mode [test-filename]
  (let [sentences (read-mst-format-file test-filename)]
    (dorun (map (fn [[idx sent]] (save-dependency-tree sent (str idx ".pdf")))
                (map-indexed #(vector %1 %2) sentences)))))

(defn -main [& args]
  (let [[options rest-args banner] (get-cli-opts args)]
    (set-logger!
     :level (:logging-level options)
     :out (org.apache.log4j.FileAppender.
           (org.apache.log4j.EnhancedPatternLayout. "%d [%p] %c %m%n")
           (str "logs/" (.format (SimpleDateFormat. "yyyy-MM-dd-HH-mm-ss") (java.util.Date.)) ".log")
           true))
    (debug (str "Command line args: " (apply str (interpose " " args))))
    (debug (str "Options: " options))
    (debug (str "Rest args: " rest-args))
    (debug (str "Host: " (.getHostName (java.net.InetAddress/getLocalHost))))
    (when (:help options)
      (println banner)
      (System/exit 0))
    (cond (= "train" (:mode options)) (train-model options)
          (= "test" (:mode options)) (test-mode options)
          (= "eval" (:mode options)) (evaluate-sentences options)
          (= "export" (:mode options)) (export-mode (:test-filename options))
          :else nil)
    (shutdown-agents)))
