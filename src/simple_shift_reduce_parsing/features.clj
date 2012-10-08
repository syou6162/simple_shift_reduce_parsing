(ns simple_shift_reduce_parsing.features
  (:use simple_shift_reduce_parsing.feature))

(deffeature-fn one-minus-word-feature (fn [x] (- x 2)) :surface)
(deffeature-fn one-minus-pos-feature (fn [x] (- x 2)) :pos-tag)

(deffeature-fn zero-minus-word-feature dec :surface)
(deffeature-fn zero-minus-pos-feature dec :pos-tag)

(deffeature-fn zero-plus-word-feature :surface)
(deffeature-fn zero-plus-pos-feature :pos-tag)

(deffeature-fn one-plus-word-feature inc :surface)
(deffeature-fn one-plus-pos-feature inc :pos-tag)