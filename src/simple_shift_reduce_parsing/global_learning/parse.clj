(ns simple_shift_reduce_parsing.global_learning.parse
  (:use [simple_shift_reduce_parsing.local_learning.parse
         :only (get-score)])
  (:use [simple_shift_reduce_parsing.global_learning.perceptron
         :only (inner-product)])
  (:require [simple_shift_reduce_parsing.action :as action])
  (:require [simple_shift_reduce_parsing.feature :as common-feature])
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:import [simple_shift_reduce_parsing.configuration Configuration]))

(defn get-fv [fv ^long act-id]
  (->> fv
       (reduce
        (fn [result ^String k]
          (let [id (common-feature/feature-to-id (str act-id "-and-" k))]
            (assoc result id 1)))
        {})))

(defn error-count [sentence ^Configuration config]
  (reduce
   (fn [result [modifier head]]
     (if (= (:head (nth sentence (:idx modifier))) ;; gold head
            (:idx head)) ;; predict head
       result
       (inc result)))
   0.0 (-> config :relations :modifier-to-head)))

(defn expand [weight sentence ^Configuration config]
  (letfn [(action-with-meta [fv' act-id]
            (let [^Configuration next-config ((action/id2action act-id) config)
                  fv (get-fv fv' act-id)
                  score (+ (-> config meta (get :score 0.0))
                           (inner-product weight fv))]
              (with-meta
                next-config
                (-> (meta config)
                    (assoc :prev-config (with-meta config {}))
                    (assoc :score score)
                    (update-in [:fv] conj fv)))))]
    (let [fv (vec (common-feature/get-fv config))
          reduce-act-id 2]
      (if (empty? (:input config))
        [(action-with-meta fv reduce-act-id)]
        (->> (action/get-possible-actions config)
             (mapv #(action-with-meta fv %)))))))

(defn parse'
  "Beam searchを使ってgreedyに探索してdecodeする関数。
  各actionで発火した素性をメタデータとして付与。"
  [get-score' weight beam-size sentence]
  (loop [cnt 0
         beam [(config/make-Configuration sentence)]]
    (if (> cnt (dec (* 2 (dec (count sentence))))) ;; 2n-1
      (let [^Configuration config (first beam)
            pairs (-> config :relations :modifier-to-head)]
        (with-meta
          (reduce (fn [sent [modifier head]]
                    (assoc-in sent [(:idx modifier) :head] (:idx head)))
                  sentence pairs)
          (-> (meta sentence)
              (assoc :fv (apply merge-with + (-> config meta :fv)))
              (assoc :history (-> config meta :history)))))
      (let [new-beam (->> beam
                          (mapv (fn [^Configuration config]
                                  (expand weight sentence config)))
                          (reduce into [])
                          (sort-by (fn [config] (get-score' sentence config)) >)
                          (take beam-size)
                          (vec))]
        (recur (inc cnt) new-beam)))))

(defn parse [weight beam-size sentence]
  (parse' (fn [_ config]
            (get-score config))
          weight beam-size sentence))

(defn parse-for-training [weight beam-size sentence]
  (parse' (fn [sent config]
            (+ (get-score config)
               (error-count sent (-> config meta (get :prev-config)))))
          weight beam-size sentence))