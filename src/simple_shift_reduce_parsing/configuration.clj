(ns simple_shift_reduce_parsing.configuration
  (:refer-clojure :exclude [reduce])
  (:use simple_shift_reduce_parsing.word))

(defrecord Configuration [stack input relation])

(defn rcomp [& fs]
  (apply comp (reverse fs)))

(defn left [{old-stack :stack
             old-input :input
             old-relation :relation
             :as configuration}]
  (let [n (peek old-stack)
        n' (first old-input)
        stack (pop old-stack)
        relation (assoc old-relation n' n)]
    (-> configuration
        (assoc :stack stack)
        (assoc :relation relation))))

(defn right [{old-stack :stack
              old-input :input
              old-relation :relation
              :as configuration}]
  (let [n (peek old-stack)
        n' (first old-input)
        stack (conj old-stack n')
        input (vec (rest old-input))
        relation (assoc old-relation n n')]
    (-> configuration
        (assoc :stack stack)
        (assoc :input input)
        (assoc :relation relation))))

(defn reduce [{old-stack :stack
               old-input :input
               old-relation :relation
               :as configuration}]
  (let [stack (pop old-stack)]
    (-> configuration
        (assoc :stack stack))))

(defn shift [{old-stack :stack
              old-input :input
              old-relation :relation
              :as configuration}]
  (let [n (first old-input)
        stack (conj old-stack n)
        input (vec (rest old-input))]
    (-> configuration
        (assoc :stack stack)
        (assoc :input input))))

(defn- get-gold-actions' [{stack :stack
                           input :input
                           relation :relation
                           :as configuration}
                          actions]
  (if (empty? input)
    actions
    (let [n (peek stack)
          n' (first input)]
      (if (> (:head n') (:idx n'))
        (if (= (:head n) (:idx n'))
          (recur (left configuration)
                 (conj actions :left))
          (recur (shift configuration)
                 (conj actions :shift)))
        (if (= (:head n') (:idx n))
          (recur (right configuration)
                 (conj actions :right))
          (recur (reduce configuration)
                 (conj actions :reduce)))))))

(defn get-gold-actions [{stack :stack
                         input :input
                         relation :relation
                         :as configuration}]
  (get-gold-actions' (shift configuration) [:shift]))
