(ns simple_shift_reduce_parsing.global_learning.feature
  (:use [simple_shift_reduce_parsing.global_learning.parse :only (get-fv)])
  (:require [simple_shift_reduce_parsing.action :as action])
  (:require [simple_shift_reduce_parsing.feature :as common-feature])
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:import [simple_shift_reduce_parsing.configuration Configuration]))

(defn with-oracle-fv [sentence]
  (let [^Configuration init-config (config/make-Configuration sentence)
        actions (->> (lazy-cat (action/get-gold-actions init-config)
                               (repeat :reduce))
                     (take (dec (* 2 (dec (count sentence)))))
                     (vec))]
    (loop [^Configuration config init-config
           actions-idx 0
           result {}]
      (if (= actions-idx (count actions))
        (with-meta
          sentence
          (-> (meta sentence)
              (assoc :fv result)))
        (let [current-action (nth actions actions-idx)
              fv (get-fv (common-feature/get-fv config)
                         (action/action2id current-action))
              ^Configuration next-config ((action/action-mapping current-action) config)]
          (recur next-config (inc actions-idx) (merge-with + result fv)))))))
