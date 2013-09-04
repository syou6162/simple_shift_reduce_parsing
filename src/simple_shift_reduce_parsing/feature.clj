(ns simple_shift_reduce_parsing.feature
  (:use [clj-utils.io :only (serialize deserialize)])
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:require [simple_shift_reduce_parsing.action :as action]))

(import '[simple_shift_reduce_parsing.configuration Configuration])

(defstruct feature :type :str)

(let [mapping (atom {})]
  (defn feature-to-id [feature]
    (let [v (get @mapping feature)
          max-id (count @mapping)]
      (if v
        v
        (do (swap! mapping assoc feature max-id)
            max-id))))
  (defn save-feature-to-id [filename]
    (serialize @mapping filename))
  (defn load-feature-to-id [filename]
    (reset! mapping (deserialize filename)))
  (defn clear-feature-to-id! []
    (reset! mapping {})))

(defn get-stack-idx [stack idx]
  (let [n (count stack)]
    (- n (- idx))))

(defn get-fv [configuration]
  (let [raw-fv (->> (seq @feature-names)
                    (map (fn [feature-fn] (feature-fn configuration)))
                    (flatten)
                    (filter (fn [fv] (not (nil? (:str fv))))))]
    (->> raw-fv
         (map feature-to-id)
         (map #(vector % 1))
         (vec))))

(defn generate-gold [sentence]
  (let [init-config (config/make-Configuration sentence)
        actions (action/get-gold-actions init-config)]
    (loop [config init-config
           actions-idx 0
           result []]
      (if (= actions-idx (count actions))
        result
        (let [current-action (nth actions actions-idx)
              next-config ((action/action-mapping current-action) config)]
          (recur
           next-config
           (inc actions-idx)
           (conj result [(action/action2id current-action)
                         (get-fv config)])))))))
