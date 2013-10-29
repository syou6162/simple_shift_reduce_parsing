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

; "w = w + alpha (phi(x_i, y_i) - phi(x_i, hat{y_i}))"
(defn update-weight [weight step-size gold prediction]
  (if (= (map :head gold) (map :head prediction))
    weight
    (->> (get-fv-diff gold prediction)
         (mapv (fn [[k v]] [k (* step-size v)]))
         (reduce (fn [result [fv-idx v]]
                   (assoc result fv-idx (+ v (get result fv-idx 0.0))))
                 weight))))

(defn add-weight [w1 w2]
  (reduce
   (fn [result [k v]]
     (assoc result k (+ (get result k 0.0) v)))
   w1 w2))
