(ns simple_shift_reduce_parsing.local_learning.parse
  (:use [simple_shift_reduce_parsing.local_learning.feature])
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:require [simple_shift_reduce_parsing.action :as action])
  (:use [liblinear.core
         :only (classify predict-probability predict-class-probability)]))

(import '[simple_shift_reduce_parsing.configuration Configuration])
(import '[de.bwaldvogel.liblinear Model])

(defn parse [^Model model sentence]
  (loop [^Configuration config (config/make-Configuration sentence)]
    (if (empty? (:input config))
      (let [pairs (-> config :relations :modifier-to-head)]
        (reduce (fn [sent [modifier head]]
                  (assoc-in sent [(:idx modifier) :head] (:idx head)))
                sentence
                pairs))
      (let [action-id (classify model (get-fv config))]
        (recur ((action/id2action action-id) config))))))

(defn get-score [config]
  (-> config meta (get :score 0.0)))

(defn expand [^Model model sentence ^Configuration config]
  (if (empty? (:input config))
    [config]
    (let [fv (get-fv config)
          probs (predict-probability model fv)
          labels (vec (. model getLabels))
          pairs (mapv vector labels probs)]
      (->> (range (count action/action-mapping))
           (mapv
            (fn [act-id]
              (let [prob (->> pairs
                              (filter (fn [[l p]] (= l act-id)))
                              (first)
                              (second)
                              (Math/log))
                    next-config ((action/id2action act-id) config)
                    cum-score (+ (get-score config) prob)]
                (with-meta
                  next-config
                  (assoc (meta config) :score cum-score)))))))))

(defn k-best-parse [^Model model k sentence]
  (loop [k-best [(config/make-Configuration sentence)]]
    (if (every? (fn [^Configuration config] (empty? (:input config))) k-best)
      (let [^Configuration config (first k-best)
            pairs (-> config :relations :modifier-to-head)]
        (reduce (fn [sent [modifier head]]
                  (assoc-in sent [(:idx modifier) :head] (:idx head)))
                sentence
                pairs))
      (let [new-k-best (->> k-best
                            (mapv (fn [^Configuration config]
                                    (expand model sentence config)))
                            (reduce into [])
                            (sort-by get-score >)
                            (take k)
                            (vec))]
        (recur new-k-best)))))
