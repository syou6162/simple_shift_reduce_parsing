(ns simple_shift_reduce_parsing.feature
  (:use clojure.set))

(defstruct feature :type :str)
(def feature-names (atom #{}))
(def children-feature-names (atom #{}))

(defmacro def-feature-fn
  ([feature-name idx-op type]
     `(let [name# (defn ~feature-name [sentence# idx#]
                    (struct
                     feature
                     '~feature-name
                     (get-in sentence# [(~idx-op idx#) ~type])))]
        (swap! feature-names conj name#))))

(defmacro def-children-feature-fn
  ([feature-name idx-op type]
     `(let [name# (defn ~feature-name [sentence# idx#]
                    (let [head# (get-in sentence# [(~idx-op idx#)])]
                      (if (nil? head#)
                        '()
                        (map
                         (fn [word#]
                          (struct
                           feature
                           '~feature-name
                           (word# ~type)))
                         (head# :modifiers)))))]
        (swap! children-feature-names conj name#))))

(def-feature-fn two-minus-word-feature (fn [x] (- x 3)) :surface)
(def-feature-fn two-minus-pos-feature (fn [x] (- x 3)) :pos-tag)

(def-children-feature-fn two-minus-children-word-features (fn [x] (- x 3)) :surface)
(def-children-feature-fn two-minus-children-pos-features (fn [x] (- x 3)) :pos-tag)

(def-feature-fn one-minus-word-feature (fn [x] (- x 2)) :surface)
(def-feature-fn one-minus-pos-feature (fn [x] (- x 2)) :pos-tag)

(def-children-feature-fn one-minus-children-word-features (fn [x] (- x 2)) :surface)
(def-children-feature-fn one-minus-children-pos-features (fn [x] (- x 2)) :pos-tag)

(def-feature-fn zero-minus-word-feature dec :surface)
(def-feature-fn zero-minus-pos-feature dec :pos-tag)

(def-children-feature-fn zero-minus-children-word-features dec :surface)
(def-children-feature-fn zero-minus-children-pos-features dec :pos-tag)

(def-feature-fn zero-plus-word-feature identity :surface)
(def-feature-fn zero-plus-pos-feature identity :pos-tag)

(def-children-feature-fn zero-plus-children-word-features identity :surface)
(def-children-feature-fn zero-plus-children-pos-features identity :pos-tag)

(def-feature-fn one-plus-word-feature inc :surface)
(def-feature-fn one-plus-pos-feature inc :pos-tag)

(def-children-feature-fn one-plus-children-word-features inc :surface)
(def-children-feature-fn one-plus-children-pos-features inc :pos-tag)

(def-feature-fn two-plus-word-feature (fn [x] (+ x 2)) :surface)
(def-feature-fn two-plus-pos-feature (fn [x] (+ x 2)) :pos-tag)

(def-children-feature-fn two-plus-children-word-features (fn [x] (+ x 2)) :surface)
(def-children-feature-fn two-plus-children-pos-features (fn [x] (+ x 2)) :pos-tag)

(defn get-fv [sentence idx]
  (->> (seq (union @feature-names @children-feature-names))
       (map (fn [feature-fn] (feature-fn sentence idx)))
       (flatten)
       (filter (fn [fv] (not (nil? (:str fv)))))
       (map #(vector % 1))
       (vec)))