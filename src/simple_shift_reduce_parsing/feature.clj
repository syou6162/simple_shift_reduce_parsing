(ns simple_shift_reduce_parsing.feature)

(defstruct feature :type :str)

(defmacro deffeature-fn
  ([feature-name idx-op type]
     `(defn ~feature-name [sentence# idx#]
	(struct
	 feature
	 '~feature-name
	 (get-in sentence# [(~idx-op idx#) ~type]))))
  ([feature-name type]
     `(deffeature-fn ~feature-name identity ~type)))

(ns simple_shift_reduce_parsing.features
  (:use simple_shift_reduce_parsing.feature))

(deffeature-fn zero-minus-word-feature dec :surface)
(deffeature-fn zero-minus-pos-feature dec :pos-tag)
(deffeature-fn zero-plus-word-feature :surface)
(deffeature-fn zero-plus-pos-feature :pos-tag)

