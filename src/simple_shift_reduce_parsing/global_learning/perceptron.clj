(ns simple_shift_reduce_parsing.global_learning.perceptron
  (:import [simple_shift_reduce_parsing.word Word]))

(defn inner-product [weight fv]
  (reduce
   (fn [result [fv-idx v]]
     (+ result (* (get weight fv-idx 0.0) v)))
   0.0 fv))

(defn norm [fv]
  (reduce
   (fn [result [k v]] (+ result (* v v)))
   0.0 fv))

(defn error-count [gold prediction]
  (->> (map vector (rest gold) (rest prediction))
       (remove (fn [[^Word x ^Word y]] (= (.head x) (.head y))))
       (count)))

(defn get-fv-diff [gold prediction]
  (merge-with +
              (-> gold meta :fv)
              (->> (-> prediction meta :fv)
                   (map (fn [[k v]] [k (- v)]))
                   (into {}))))

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

(defn update-weight [weight diff scale]
  (->> diff
       (mapv (fn [[k v]] [k (* scale v)]))
       (reduce (fn [result [fv-idx v]]
                 (assoc result fv-idx (+ v (get result fv-idx 0.0))))
               weight)))

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
