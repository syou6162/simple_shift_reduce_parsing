(ns simple_shift_reduce_parsing.global_learning.perceptron
  (:use [simple_shift_reduce_parsing.global_learning.parse
         :only (parse parse-for-training)])
  (:use [simple_shift_reduce_parsing.global_learning.numeric
         :only (inner-product norm)])
  (:import [simple_shift_reduce_parsing.word Word])
  (:use [clj-utils.random :only (shuffle-with-random)]))

(defn error-count [gold prediction]
  (->> (map vector (rest gold) (rest prediction))
       (remove (fn [[^Word x ^Word y]] (= (.head x) (.head y))))
       (count)))

(defn get-fv-diff [gold prediction]
  (let [tuples [(-> gold meta :history)
                (-> prediction meta :history)
                (iterate inc 0)]
        [_ _ first-wrong-idx] (->> (apply map vector tuples)
                                   (drop-while (fn [[g p idx]] (= g p)))
                                   (first))]
    (if (nil? first-wrong-idx)
      {} ;; completely same action sequence
      (->> (map vector
                (->> gold meta :fv (drop first-wrong-idx))
                (->> prediction meta :fv (drop first-wrong-idx)))
           (reduce
            (fn [result [g p']]
              (let [p (->> p'
                           (map (fn [[k v]] [k (- v)]))
                           (into {}))]
                (merge-with + result g p)))
            {})))))

(defn get-step-size
  [weight gold prediction fv-diff]
  (let [num-errors (error-count gold prediction)
        in-prod (inner-product weight fv-diff)
        n (norm fv-diff)
        step-size (if (zero? n)
                    ;; goldなものでなくてもfvの次元ではexactに一致し
                    ;; normが0になってしまう場合が存在する。
                    ;; その場合0割を防ぐ必要がある
                    0.0
                    (->> (/ (- num-errors in-prod) n)
                         (max 0.0)))]
    step-size))

(defn update-weight'
  "w = w + scale * (gold - prediction)"
  [weight diff scale]
  (->> diff
       (mapv (fn [[k v]] [k (* scale v)]))
       (reduce (fn [result [fv-idx v]]
                 (assoc result fv-idx (+ v (get result fv-idx 0.0))))
               weight)))

(use '[simple_shift_reduce_parsing.global_learning.parse
         :only (parse parse-for-training)])

(defn update-weight [{iter :iter
                      beam-size :beam-size
                      weight :weight
                      cum-weight :cum-weight
                      training-sentences :training-sentences}]
  (let [n (count training-sentences)]
    (->> (range n)
         (reduce
          (fn [[w cum-w] idx]
            (let [gold (nth training-sentences idx)
                  prediction (parse-for-training w beam-size gold)
                  fv-diff (get-fv-diff gold prediction)
                  step-size (get-step-size w gold prediction fv-diff)
                  diff (mapv (fn [[k v]] [k (* step-size v)]) fv-diff)
                  new-w (update-weight' w diff 1.0)
                  t (+ (* iter n) (inc idx))
                  new-cum-w (update-weight' new-w diff t)]
              [new-w new-cum-w]))
          [weight cum-weight])
         (time))))

(defn get-averaged-weight
  "w = w_final - w_a / t"
  [cum-count weight cum-weight]
  (if (or (zero? cum-count) (neg? cum-count))
    weight
    (->> cum-weight
         (reduce
          (fn [result [k v]]
            (let [new-v (- (get result k 0.0) (/ v cum-count))]
              (assoc result k new-v)))
          weight))))
