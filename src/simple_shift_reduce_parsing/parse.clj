(ns simple_shift_reduce_parsing.parse
  (:use [simple_shift_reduce_parsing.feature])
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:require [simple_shift_reduce_parsing.action :as action])
  (:use [fobos-multiclass-clj.multiclass :only (argmax-label)]))

(import '[simple_shift_reduce_parsing.configuration Configuration])

(defn parse [models sentence]
  (loop [config (config/make-Configuration sentence)]
    (if (empty? (:input config))
      (let [pairs (-> config :relations :modifier-to-head)]
        (reduce (fn [sent [modifier head]]
                  (assoc-in sent [(:idx modifier) :head] (:idx head)))
                sentence
                pairs))
      (let [action (argmax-label models (get-fv config))]
        (recur ((action/action-mapping action) config))))))
