(ns simple_shift_reduce_parsing.global_learning.numeric)

(defn inner-product [^doubles weight fv]
  (reduce
   (fn [result [fv-idx v]]
     (+ result (* (aget weight fv-idx) v)))
   0.0 fv))

(defn norm [fv]
  (reduce
   (fn [result [k v]] (+ result (* v v)))
   0.0 fv))
