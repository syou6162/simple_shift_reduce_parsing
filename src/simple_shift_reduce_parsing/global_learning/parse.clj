(ns simple_shift_reduce_parsing.global_learning.parse
  (:use [simple_shift_reduce_parsing.local_learning.parse
         :only (get-score)])
  (:use [simple_shift_reduce_parsing.global_learning.perceptron
         :only (inner-product)])
  (:use [simple_shift_reduce_parsing.hash-feature :only (mod-feature-id)])
  (:require [simple_shift_reduce_parsing.action :as action])
  (:require [simple_shift_reduce_parsing.hash-feature
             :as common-feature])
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:import [simple_shift_reduce_parsing.configuration Configuration])
  (:import [simple_shift_reduce_parsing.word Word]))

(import Jenkins)

(defn get-fv [fv ^long act-id]
  (->> fv
       (reduce
        (fn [result f]
          (let [id (mod-feature-id (Jenkins/hashPair (int f) (int act-id)))]
            (assoc result id (inc (get result id 0)))))
        {})))

(defn error-count [sentence ^Configuration config]
  (reduce
   (fn [result [modifier-idx head-idx]]
     (let [^Word w (nth sentence modifier-idx)]
       (if (= (.head w) head-idx)
         result
         (inc result))))
   0.0 (-> config :relations :modifier-to-head)))

(defn expand [^Configuration config weight fv]
  (letfn [(action-with-meta [act-id]
            (let [next-action (action/id2action act-id)
                  ^Configuration next-config (next-action config)
                  fv (get-fv fv act-id)
                  score (+ (-> config meta (get :score 0.0))
                           (inner-product weight fv))]
              (with-meta
                next-config
                (-> (meta config)
                    (assoc :prev-config (with-meta config {}))
                    (assoc :score score)
                    (update-in [:fv] conj fv)))))]
    (let [reduce-act-id 2]
      (if (empty? (:input config))
        [(action-with-meta reduce-act-id)]
        (->> (action/get-possible-actions config)
             (mapv action-with-meta))))))

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
              (assoc :fv (apply merge-with + (-> config meta :fv))))))
      (let [new-beam (->> beam
                          (map (fn [^Configuration config]
                                 (let [fv (common-feature/get-fv config)]
                                   (expand config weight fv))))
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
