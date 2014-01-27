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

(defn update-weight!'
  "w = w + scale * (gold - prediction)"
  [^doubles weight diff scale]
  (doseq [[fv-idx v] (->> diff
                          (mapv (fn [[k v]] [k (* scale v)])))]
    (aset weight fv-idx (+ v (aget weight fv-idx)))))

(defn update-weight! [{iter :iter
                       beam-size :beam-size
                       weight :weight
                       cum-weight :cum-weight
                       training-sentences :training-sentences}]
  (let [n (count training-sentences)
        sents (shuffle-with-random training-sentences)]
    (doseq [idx (range n)]
      (let [gold (nth sents idx)
            prediction (parse-for-training weight beam-size gold)
            fv-diff (get-fv-diff gold prediction)
            step-size (get-step-size weight gold prediction fv-diff)
            diff (mapv (fn [[k v]] [k (* step-size v)]) fv-diff)
            t (+ (* iter n) (inc idx))]
        (update-weight!' weight diff 1.0)
        (update-weight!' cum-weight diff t)))))

(defn get-averaged-weight
  "w = w_final - w_a / t"
  [cum-count ^doubles weight ^doubles cum-weight]
  (if (or (zero? cum-count) (neg? cum-count))
    weight
    (amap weight idx ret
          (- (aget weight idx) (/ (aget cum-weight idx) cum-count)))))
