(ns simple_shift_reduce_parsing.configuration
  (:refer-clojure :exclude [reduce])
  (:use simple_shift_reduce_parsing.word))

(import '[simple_shift_reduce_parsing.word Word])

(defrecord Configuration
    [^clojure.lang.PersistentVector sentence
     ^clojure.lang.PersistentVector stack
     ^clojure.lang.PersistentQueue input
     relations])

(defn make-Configuration [input]
  (Configuration. input []
                  (->> (mapv :idx input)
                       (apply conj clojure.lang.PersistentQueue/EMPTY))
                  {:head-to-modifiers {} :modifier-to-head {}}))

(defn add-dependency-arc
  [^Configuration config head-idx modifier-idx]
  (let [relations (:relations config)
        new-relations (-> relations
                          (update-in [:head-to-modifiers head-idx]
                                     (comp vec conj) modifier-idx)
                          (update-in [:head-to-modifiers head-idx]
                                     (comp vec sort))
                          (assoc-in [:modifier-to-head modifier-idx]
                                    head-idx))]
    (assoc config :relations new-relations)))
