(ns simple_shift_reduce_parsing.action
  (:use simple_shift_reduce_parsing.configuration)
  (:refer-clojure :exclude [reduce]))

(defn shift [{old-stack :stack
              old-input :input
              :as configuration}]
  "Shift operation for configuration.
   <S, n | I, A> => <n | S, I , A>"
  (assert (not (empty? old-input)))
  (let [n (first old-input)
        stack (conj old-stack n)
        input (vec (rest old-input))]
    (-> configuration
        (assoc :stack stack)
        (assoc :input input))))

(defn reducable? [config]
  (or (not (empty? (:stack config)))
      (contains? (-> config :relations :modifier-to-head)
                 (peek (:stack config)))))

(defn reduce [{old-stack :stack
               old-input :input
               :as configuration}]
  "Reduce operation for configuration
   <n | S, I , A> => <S, I, A>"
  (if (reducable? configuration)
    (let [stack (pop old-stack)]
      (-> configuration
          (assoc :stack stack)))
    (shift configuration)))

(defn leftable? [config]
  (and (not (empty? (:stack config)))
       (not (contains? (-> config :relations :modifier-to-head)
                       (peek (:stack config))))))

(defn left [{old-stack :stack
             old-input :input
             old-relations :relations
             :as config}]
  "Left operation for configuration
   <n | S, n' | I, A> => <S, n' | I, A \\cup {(n', n)}>"
  (if (leftable? config)
    (let [n (peek old-stack) ; modifier
          n' (first old-input) ; head
          stack (pop old-stack)
          relations (:relations (add-dependency-arc config n' n))]
      (assert (not (empty? old-input)))
      (-> config
          (assoc :stack stack)
          (assoc :relations relations)))
    (reduce config)))

(defn rightable? [config]
  (and (not (empty? (:stack config)))
       (not (contains? (-> config :relations :modifier-to-head)
                       (first (-> config :input))))))

(defn right [{old-stack :stack
              old-input :input
              old-relation :relation
              :as config}]
  (if (rightable? config)
    (let [n (peek old-stack) ; head
          n' (first old-input) ; modifier
          stack (conj old-stack n')
          input (vec (rest old-input))
          relations (:relations (add-dependency-arc config n n'))]
      (assert (not (empty? old-input)))
      (-> config
          (assoc :stack stack)
          (assoc :input input)
          (assoc :relations relations)))
    (reduce config)))

(def action-mapping {:left left
                     :right right
                     :reduce reduce
		     :shift shift})

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

(defn get-gold-actions [config]
  (get-gold-actions' (shift config) [:shift]))
