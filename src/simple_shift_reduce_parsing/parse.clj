(ns simple_shift_reduce_parsing.parse
  (:use [simple_shift_reduce_parsing.feature])
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:require [simple_shift_reduce_parsing.action :as action])
  (:use [liblinear.core :only (classify)]))

(import '[simple_shift_reduce_parsing.configuration Configuration])

(defn parse [models sentence]
  (loop [config (config/make-Configuration sentence)]
    (if (empty? (:input config))
      (let [pairs (-> config :relations :modifier-to-head)]
        (reduce (fn [sent [modifier head]]
                  (assoc-in sent [(:idx modifier) :head] (:idx head)))
                sentence
                pairs))
      (let [action-id (classify models (get-fv config))]
        (recur ((action/id2action action-id) config))))))
