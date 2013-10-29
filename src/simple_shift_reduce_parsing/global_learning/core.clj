(ns simple_shift_reduce_parsing.global_learning.core
  (:use [simple_shift_reduce_parsing.util :only (read-mst-format-file)])
  (:use [simple_shift_reduce_parsing.global_learning.perceptron
         :only (get-fv-diff get-step-size update-weight add-weight)])
  (:use [simple_shift_reduce_parsing.global_learning.feature
         :only (with-oracle-fv)])
  (:use [simple_shift_reduce_parsing.global_learning.parse
         :only (parse parse-for-training)])
  (:use [simple_shift_reduce_parsing.evaluation
         :only (get-dependency-accuracy)])
  (:use [clj-utils.random :only (shuffle-with-random)]))

(import java.text.SimpleDateFormat)
(import (org.apache.log4j PatternLayout FileAppender))
(use 'clojure.tools.logging 'clj-logging-config.log4j)
(require '[clojure.tools.cli :as cli])

(defn- get-cli-opts [args]
  (cli/cli args
           ["-h" "--help" "Show help" :default false :flag true]
           ["--beam-size" "Size of beam" :default 1 :parse-fn #(Integer. %)]
           ["--mode" "(train|test|eval|export)"]
	   ["--training-filename" "File name of training" :default "train.txt"]
           ["--dev-filename" "File name of dev" :default "dev.txt"]
           ["--test-filename" "File name of test" :default "test.txt"]
           ["--model-filename" "File name of the (saved|load) model" :default "parsing.model"]
           ["--logging-level" "level of logging" :default :debug :parse-fn #(keyword %)]
           ["--feature-to-id-filename" "File name of the feature2id mapping" :default "feature-to-id.bin"]))

(defn train-model [{training-filename :training-filename
                    dev-filename :dev-filename
                    model-filename :model-filename
                    feature-to-id-filename :feature-to-id-filename
                    beam-size :beam-size}]
  (let [training-sentences (->> training-filename
                                (read-mst-format-file)
                                (shuffle-with-random)
                                (vec))
        training-sentences-with-oracle-fv (mapv with-oracle-fv training-sentences)
        dev-sentences (->> dev-filename
                           (read-mst-format-file)
                           (shuffle-with-random)
                           (vec))]
    (loop [iter 0
           weight {}
           cum-weight {}]
      (let [[new-weight new-cum-weight] (->> training-sentences-with-oracle-fv
                                             (reduce
                                              (fn [[w cum-w] gold]
                                                (let [decode (partial parse-for-training w beam-size)
                                                      prediction (decode gold)
                                                      fv-diff (get-fv-diff gold prediction)
                                                      step-size (get-step-size w gold prediction fv-diff)
                                                      new-w (update-weight w step-size gold prediction)
                                                      new-cum-w (add-weight cum-w new-w)]
                                                  [new-w new-cum-w]))
                                              [weight cum-weight]))
            decode (partial parse new-cum-weight beam-size)]
        (->> (str iter ", "
                  (get-dependency-accuracy training-sentences (mapv decode training-sentences))
                  ", "
                  (get-dependency-accuracy dev-sentences (mapv decode dev-sentences)))
             (println))
        (recur (inc iter) new-weight new-cum-weight)))))

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
          :else nil)
    (shutdown-agents)))
