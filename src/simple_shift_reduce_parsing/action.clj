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
        (assoc :input input)
        (vary-meta update-in [:prev-actions] conj :shift))))

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
          (assoc :stack stack)
          (vary-meta update-in [:prev-actions] conj :reduce)))
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
          (assoc :relations relations)
          (vary-meta update-in [:prev-actions] conj :left)))
    (reduce config)))

(defn rightable? [config]
  (and (not (empty? (:stack config)))
       (not (contains? (-> config :relations :modifier-to-head)
                       (first (-> config :input))))))

(defn right [{old-stack :stack
              old-input :input
              old-relation :relation
              :as config}]
  "Right operation for configuration
   <n | S, n' | I, A> => <n' | n | S, I, A \\cup {(n, n')}>"
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
          (assoc :relations relations)
          (vary-meta update-in [:prev-actions] conj :right)))
    (reduce config)))

(def action-mapping {:left left
                     :right right
                     :reduce reduce
		     :shift shift})

; reference http://tinyurl.com/97jx424

;; # This function implements the rules described in section 4.1.
;; def find_correct_action(parser, gold_tree):
;;     """For a parser in some state, find the next action to carry out."""

;;     # Shouldn't call this function if queue is empty...
;;     if len(parser.queue) == 0:
;;         return None
;;     # Rule 1: If empty stack, return SHIFT
;;     if len(parser.stack) == 0:
;;         return "shift"
;;     T = parser.stack[-1] #
;;     F = parser.queue[0] #

;;     # TG/FG are the counterparts of T/F in the gold tree
;;     TG = gold_tree.tokens[T.position]
;;     FG = gold_tree.tokens[F.position]

;;     # Rule 2: If gold tree has TG <- FG, then return LEFT_ARC
;;     if not TG.isdummy and TG.head == FG:
;;         return "left_arc"
;;     # Rule 3: If gold tree has TG -> FG, then return RIGHT_ARC
;;     if FG.head == TG:
;;         return "right_arc"
;;     # Rule 4: If stack contains token A, such that its counterpart AG
;;     #         in the gold tree has AG -> FG or AG <- FG, then REDUCE
;;     for A in parser.stack:
;;         AG = gold_tree.tokens[A.position]
;;         if FG.head == AG or AG.head == FG:
;;             return "reduce"
;;     # Rule 5: Default: return SHIFT
;;     return "shift"

(defn next-gold-action [{stack :stack input :input}]
  (if (empty? stack)
    :shift
    (let [n (peek stack)
          n' (first input)]
      (cond
       (= (:head n) (:idx n')) :left
       (= (:head n') (:idx n)) :right
       (some (fn [word]
               (or (= (:head n') (:idx word))
                   (= (:head word) (:idx n')))) stack)
       :reduce
       :else :shift))))

(defn- get-gold-actions' [{input :input :as config} actions]
  (if (empty? input)
    actions
    (let [next-action (next-gold-action config)]
      (recur ((action-mapping next-action) config)
             (conj actions next-action)))))

(defn get-gold-actions [config]
  (get-gold-actions' config []))
