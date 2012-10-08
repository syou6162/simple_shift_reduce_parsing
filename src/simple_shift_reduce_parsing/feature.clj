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