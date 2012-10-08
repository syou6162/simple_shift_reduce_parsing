(ns simple_shift_reduce_parsing.feature)

(defstruct feature :type :str)
(def feature-names (atom #{}))

(defmacro deffeature-fn
  ([feature-name idx-op type]
     `(let [name# (defn ~feature-name [sentence# idx#]
                    (struct
                     feature
                     '~feature-name
                     (get-in sentence# [(~idx-op idx#) ~type])))]
        (swap! feature-names conj name#)))
  ([feature-name type]
     `(deffeature-fn ~feature-name identity ~type)))

(deffeature-fn one-minus-word-feature (fn [x] (- x 2)) :surface)
(deffeature-fn one-minus-pos-feature (fn [x] (- x 2)) :pos-tag)

(deffeature-fn zero-minus-word-feature dec :surface)
(deffeature-fn zero-minus-pos-feature dec :pos-tag)

(deffeature-fn zero-plus-word-feature :surface)
(deffeature-fn zero-plus-pos-feature :pos-tag)

(deffeature-fn one-plus-word-feature inc :surface)
(deffeature-fn one-plus-pos-feature inc :pos-tag)

(defn get-fv [sentence idx]
  (->> (seq @feature-names)
       (map (fn [feature-fn] (vector (feature-fn sentence idx) 1)))
       (vec)))
