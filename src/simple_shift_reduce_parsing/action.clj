(ns simple_shift_reduce_parsing.action
  (:use simple_shift_reduce_parsing.configuration)
  (:refer-clojure :exclude [reduce]))

(import '[simple_shift_reduce_parsing.word Word])
(import '[simple_shift_reduce_parsing.configuration Configuration])

(defn shift [^Configuration config]
  "Shift operation for configuration.
   <S, n | I, A> => <n | S, I , A>"
  (let [n (int (peek (.input config)))]
    (-> config
        (update-in [:stack] conj n)
        (update-in [:input] pop)
        (vary-meta update-in [:history] (comp vec conj) :shift))))

(defn reducable? [^Configuration config]
  (or (not (zero? (count (.stack config))))
      (contains? (-> config :relations :modifier-to-head)
                 (peek (.stack config)))))

(defn reduce [^Configuration config]
  "Reduce operation for configuration
   <n | S, I , A> => <S, I, A>"
  (-> config
      (update-in [:stack] pop)
      (vary-meta update-in [:history] (comp vec conj) :reduce)))

(defn leftable? [^Configuration config]
  (and (not (zero? (count (:stack config))))
       (not (zero? (count (:input config))))
       (not (contains? (-> config :relations :modifier-to-head)
                       (peek (:stack config))))))

(defn left [^Configuration config]
  "Left operation for configuration
   <n | S, n' | I, A> => <S, n' | I, A \\cup {(n', n)}>"
  (let [n (int (peek (.stack config))) ; modifier
        n' (int (peek (.input config)))] ; head
    (assert (not (zero? (count (.input config)))))
    (-> config
        (update-in [:stack] pop)
        (add-dependency-arc n' n)
        (vary-meta update-in [:history] (comp vec conj) :left))))

(defn rightable? [^Configuration config]
  (and (not (zero? (count (:stack config))))
       (not (zero? (count (:input config)))) ;; ???
       (not (contains? (-> config :relations :modifier-to-head)
                       (peek (-> config :input))))))

(defn right [^Configuration config]
  "Right operation for configuration
   <n | S, n' | I, A> => <n' | n | S, I, A \\cup {(n, n')}>"
  (let [n (int (peek (.stack config))) ; head
        n' (int (peek (.input config)))] ; modifier
    (assert (not (zero? (count (.input config)))))
    (-> config
        (update-in [:stack] conj n')
        (update-in [:input] pop)
        (add-dependency-arc n n')
        (vary-meta update-in [:history] (comp vec conj) :right))))

(def action-mapping {:left left
                     :right right
                     :reduce reduce
		     :shift shift})

(def action2id {:left 0, :right 1, :reduce 2, :shift 3})
(def id2action [#'left #'right #'reduce #'shift])

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

(defn next-gold-action [^Configuration
                        {stack :stack input :input :as config}]
  (if (empty? stack)
    :shift
    (let [^Word n (nth (:sentence config) (peek stack))
          ^Word n' (nth (:sentence config) (peek input))]
      (cond
       (= (:head n) (:idx n'))
       :left

       (= (:head n') (:idx n))
       :right

       (some (fn [word-idx]
               (let [^Word word (nth (:sentence config) word-idx)]
                 (and (or (= (:head n') (:idx word))
                          (= (:head word) (:idx n')))))) stack)
       :reduce
       :else :shift))))

(defn- get-gold-actions'
  [^Configuration {input :input :as ^Configuration config} actions]
  (if (empty? input)
    actions
    (let [next-action (next-gold-action config)]
      (recur ((action-mapping next-action) config)
             (conj actions next-action)))))

(defn get-gold-actions [^Configuration config]
  (get-gold-actions' config []))

(defn empty-input? [^Configuration config]
  (zero? (count (:input config))))

(defn empty-stack? [^Configuration config]
  (zero? (count (:stack config))))

(defn get-last-action [^Configuration config]
  (-> config meta :history last))

(defn get-possible-actions [^Configuration config]
  (let [possible-actions (atom #{})]
    (if (and
         (= (-> config :sentence count dec)
            (-> config :relations :modifier-to-head count))
         (not (empty-stack? config)))
      (swap! possible-actions conj :reduce)
      (do
        (when (and (not (= (get-last-action config) :reduce))
                   (not (empty-input? config)))
          (swap! possible-actions conj :shift))
        (when-not (empty-stack? config)
          (when (and (not (empty-input? config)))
            (swap! possible-actions conj :right))
          (if (and
               (not (contains? (-> config :relations :modifier-to-head)
                               (-> config :stack peek)))
               (not (-> config :input empty?)))
            (swap! possible-actions conj :left)
            (swap! possible-actions conj :reduce)))))
    (assert (not (empty? (vec @possible-actions))))
    (->> (vec @possible-actions)
         (mapv action2id))))
