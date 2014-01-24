(ns simple_shift_reduce_parsing.global_learning.feature
  (:use [simple_shift_reduce_parsing.global_learning.parse :only (get-fv)])
  (:require [simple_shift_reduce_parsing.action :as action])
  (:require [simple_shift_reduce_parsing.hash-feature
             :as common-feature])
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:import [simple_shift_reduce_parsing.configuration Configuration]))

(defn have-same-dependencies? [gold-sent oracle-config]
  (let [v1 (->> gold-sent
                (map
                 (fn [w]
                   (vector (:idx w) (:head w))))
                (into {}))
        v2 (-> oracle-config :relations :modifier-to-head
               (assoc 0 -1))]
    (= v1 v2)))

(defn with-oracle-fv [sentence]
  (let [^Configuration init-config (config/make-Configuration sentence)
        actions (->> (lazy-cat (action/get-gold-actions init-config)
                               (repeat :reduce))
                     (take (dec (* 2 (count sentence))))
                     (vec))]
    (loop [^Configuration config init-config
           actions-idx 0
           result []]
      (if (= actions-idx (count actions))
        (do
          (assert (have-same-dependencies? sentence config))
          (with-meta sentence (-> sentence meta
                                  (assoc :history
                                    (-> config meta :history))
                                  (assoc :fv result))))
        (let [current-action (nth actions actions-idx)
              fv (get-fv (common-feature/get-fv config)
                         (action/action2id current-action))
              ^Configuration next-config ((action/action-mapping current-action) config)]
          (-> (contains? (-> config action/get-possible-actions set)
                         (action/action2id current-action))
              assert)
          (recur next-config (inc actions-idx)
                 (conj result fv)))))))
