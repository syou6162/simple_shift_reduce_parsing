(ns simple_shift_reduce_parsing.configuration
  (:refer-clojure :exclude [reduce])
  (:use simple_shift_reduce_parsing.word))

(import '[simple_shift_reduce_parsing.word Word])

(defrecord Configuration [stack input relations])

(defn make-Configuration [input]
  (Configuration. [] input {:head-to-modifiers {} :modifier-to-head {}}))

(defn add-dependency-arc
  [^Configuration config ^Word head-node ^Word modifier-node]
  (let [relations (:relations config)
        new-relations (-> relations
                          (update-in [:head-to-modifiers head-node]
                                     (comp vec conj) modifier-node)
                          (assoc-in [:modifier-to-head modifier-node]
                                    head-node))]
    (assoc config :relations new-relations)))
