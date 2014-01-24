(ns simple_shift_reduce_parsing.local_learning.feature
  (:require [simple_shift_reduce_parsing.hash-feature :as common-feature])
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:require [simple_shift_reduce_parsing.action :as action])
  (:import [simple_shift_reduce_parsing.configuration Configuration])
  (:import [de.bwaldvogel.liblinear FeatureNode]))

(defn get-fv [^Configuration config]
  (let [tmp (->> config
                 (common-feature/get-fv)
                 (set)
                 (sort)
                 (vec))
        a (make-array FeatureNode (count tmp))
        n (count tmp)]
    (doseq [idx (range n)]
      (aset ^objects a idx (new FeatureNode (inc (nth tmp idx)) 1.0)))
    a))

(defn get-fv' [^Configuration config]
  (->> config
       (common-feature/get-fv)
       (set)
       (sort)
       (map #(vector % 1))
       (vec)))

(defn generate-gold [sentence]
  (let [^Configuration init-config (config/make-Configuration sentence)
        actions (action/get-gold-actions init-config)]
    (loop [^Configuration config init-config
           actions-idx 0
           result []]
      (if (= actions-idx (count actions))
        result
        (let [current-action (nth actions actions-idx)
              ^Configuration next-config ((action/action-mapping current-action) config)]
          (recur
           next-config
           (inc actions-idx)
           (conj result [(action/action2id current-action)
                         (get-fv' config)])))))))
