(ns simple_shift_reduce_parsing.feature)

(defstruct feature :type :str)
(def feature-names (atom #{}))

(defmacro def-feature-fn
  ([feature-name idx-op type]
     `(let [name# (defn ~feature-name [sentence# idx#]
                    (struct
                     feature
                     '~feature-name
                     (get-in sentence# [(~idx-op idx#) ~type])))]
        (swap! feature-names conj name#)))
  ([feature-name type]
     `(def-feature-fn ~feature-name identity ~type)))

(def-feature-fn two-minus-word-feature (fn [x] (- x 3)) :surface)
(def-feature-fn two-minus-pos-feature (fn [x] (- x 3)) :pos-tag)

(def-feature-fn one-minus-word-feature (fn [x] (- x 2)) :surface)
(def-feature-fn one-minus-pos-feature (fn [x] (- x 2)) :pos-tag)

(def-feature-fn zero-minus-word-feature dec :surface)
(def-feature-fn zero-minus-pos-feature dec :pos-tag)

(def-feature-fn zero-plus-word-feature :surface)
(def-feature-fn zero-plus-pos-feature :pos-tag)

(def-feature-fn one-plus-word-feature inc :surface)
(def-feature-fn one-plus-pos-feature inc :pos-tag)

(def-feature-fn two-plus-word-feature (fn [x] (+ x 2)) :surface)
(def-feature-fn two-plus-pos-feature (fn [x] (+ x 2)) :pos-tag)

(defn get-fv [sentence idx]
  (->> (seq @feature-names)
       (map (fn [feature-fn] (vector (feature-fn sentence idx) 1)))
       (vec)))
