(ns simple_shift_reduce_parsing.feature
  (:use [clj-utils.io :only (serialize deserialize)])
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:require [simple_shift_reduce_parsing.action :as action]))

;; 素性は以下の論文のBaseline feature templatesを参考に作成
;; labelに関する素性は未実装
;; http://www.sutd.edu.sg/cmsresource/faculty/yuezhang/acl11j.pdf

(import '[simple_shift_reduce_parsing.configuration Configuration])

(defstruct feature :type :str)

(let [mapping (atom {})]
  (defn feature-to-id [feature]
    (let [v (get @mapping feature)
          max-id (count @mapping)]
      (if v
        v
        (do (swap! mapping assoc feature max-id)
            max-id))))
  (defn save-feature-to-id [filename]
    (serialize @mapping filename))
  (defn load-feature-to-id [filename]
    (reset! mapping (deserialize filename)))
  (defn clear-feature-to-id! []
    (reset! mapping {})))

(defn get-stack-idx [stack idx]
  (let [n (count stack)]
    (- n (- idx))))

(defmacro def-around-feature-fn [feature-name idx type]
  `(defn ~feature-name [configuration#]
     (if (neg? ~idx)
       (let [stack# (get configuration# :stack)]
         (get-in stack# [(get-stack-idx stack# ~idx) ~type]))
       (get-in configuration# [:input ~idx ~type]))))

(defmacro def-conjunctive-feature-fn [& fs-list]
  (let [fs (vec fs-list)
        feature-name (symbol (clojure.string/join "-and-" fs))]
    `(defn ~feature-name [configuration#]
       (let [tmp# (map (fn [f#] (f# configuration#)) ~fs)]
         (if (every? #(not (nil? %)) tmp#)
           (clojure.string/join \& tmp#))))))

(def single-word-features
  [(def-around-feature-fn zero-minus-word-feature -1 :surface)
   (def-around-feature-fn zero-minus-pos-feature -1 :pos-tag)
   (def-conjunctive-feature-fn zero-minus-word-feature zero-minus-pos-feature)

   (def-around-feature-fn zero-plus-word-feature 0 :surface)
   (def-around-feature-fn zero-plus-pos-feature 0 :pos-tag)
   (def-conjunctive-feature-fn zero-plus-word-feature zero-plus-pos-feature)

   (def-around-feature-fn one-plus-word-feature 1 :surface)
   (def-around-feature-fn one-plus-pos-feature 1 :pos-tag)
   (def-conjunctive-feature-fn one-plus-word-feature one-plus-pos-feature)

   (def-around-feature-fn two-plus-word-feature 2 :surface)
   (def-around-feature-fn two-plus-pos-feature 2 :pos-tag)
   (def-conjunctive-feature-fn two-plus-word-feature two-plus-pos-feature)])

(def two-words-features
  [(def-conjunctive-feature-fn
     zero-minus-word-feature zero-minus-pos-feature
     zero-plus-word-feature zero-plus-pos-feature)

   (def-conjunctive-feature-fn
     zero-minus-word-feature zero-minus-pos-feature
     zero-plus-word-feature)

   (def-conjunctive-feature-fn
     zero-minus-word-feature
     zero-plus-word-feature zero-plus-pos-feature)

   (def-conjunctive-feature-fn
     zero-minus-word-feature zero-minus-pos-feature
     zero-plus-pos-feature)

   (def-conjunctive-feature-fn
     zero-minus-pos-feature
     zero-plus-word-feature zero-plus-pos-feature)

   (def-conjunctive-feature-fn
     zero-minus-word-feature
     zero-plus-word-feature)

   (def-conjunctive-feature-fn
     zero-minus-pos-feature
     zero-plus-pos-feature)

   (def-conjunctive-feature-fn
     zero-plus-pos-feature
     one-plus-pos-feature)])

(defmacro def-both-word-and-pos-feature [feature-name]
  (let [word-feature-name (symbol (str feature-name "-word-feature"))
        pos-feature-name (symbol (str feature-name "-pos-feature"))]
    `(do
       (defn ~word-feature-name [config#]
         (-> config# ~feature-name :surface))
       (defn ~pos-feature-name [config#]
         (-> config# ~feature-name :pos-tag)))))

(defn head-of-stack [{stack :stack, relations :relations}]
  (let [t (peek stack) ; top of the stack
        th (get-in relations [:modifier-to-head t])] ; head of t
    th))

(defn leftmost-dependent-of-stack [{stack :stack, relations :relations}]
  (let [t (peek stack) ; top of the stack
        tl (first (get-in relations [:head-to-modifiers t]))]
    tl))

(defn rightmost-dependent-of-stack [{stack :stack, relations :relations}]
  (let [t (peek stack) ; top of the stack
        tr (peek (get-in relations [:head-to-modifiers t]))]
    tr))

(defn leftmost-dependent-of-input [{input :input, relations :relations}]
  (let [n (first input) ; next input token
        nl (first (get (:head-to-modifiers relations) n))] ; leftmost dependent of n
    nl))

(def-both-word-and-pos-feature head-of-stack)
(def-both-word-and-pos-feature leftmost-dependent-of-stack)
(def-both-word-and-pos-feature rightmost-dependent-of-stack)
(def-both-word-and-pos-feature leftmost-dependent-of-input)

(def three-words-features
  [(def-conjunctive-feature-fn
     zero-plus-pos-feature one-plus-pos-feature two-plus-pos-feature)

   (def-conjunctive-feature-fn
     zero-plus-pos-feature zero-plus-pos-feature one-plus-pos-feature)

   (def-conjunctive-feature-fn
     head-of-stack-pos-feature zero-plus-pos-feature zero-plus-pos-feature)

   (def-conjunctive-feature-fn
     zero-plus-pos-feature leftmost-dependent-of-stack-pos-feature zero-plus-pos-feature)

   (def-conjunctive-feature-fn
     zero-plus-pos-feature rightmost-dependent-of-stack-pos-feature zero-plus-pos-feature)

   (def-conjunctive-feature-fn
     zero-plus-pos-feature zero-plus-pos-feature leftmost-dependent-of-input-pos-feature)])

(defn distance-feature [{stack :stack, input :input}]
  (let [i (:idx (peek stack))
        j (:idx (first input))
        dist (if (and i j)
               (let [d (Math/abs (int (- i j)))]
                 (cond (> d 10) 11
                       (> d 5) 6
                       :else d))
               -1)]
    dist))

(def distance-features
  [(def-conjunctive-feature-fn
     zero-minus-word-feature distance-feature)

   (def-conjunctive-feature-fn
     zero-minus-pos-feature distance-feature)

   (def-conjunctive-feature-fn
     zero-plus-word-feature distance-feature)

   (def-conjunctive-feature-fn
     zero-plus-pos-feature distance-feature)

   (def-conjunctive-feature-fn
     zero-minus-word-feature zero-plus-word-feature distance-feature)

   (def-conjunctive-feature-fn
     zero-minus-pos-feature zero-plus-pos-feature distance-feature)])

(def valency-features [])

(def unigram-features
  [#'head-of-stack-pos-feature
   #'head-of-stack-word-feature
   #'leftmost-dependent-of-stack-word-feature
   #'leftmost-dependent-of-stack-pos-feature
   #'rightmost-dependent-of-stack-word-feature
   #'rightmost-dependent-of-stack-pos-feature
   #'leftmost-dependent-of-input-word-feature
   #'leftmost-dependent-of-input-pos-feature])

(def third-order-features [])

(def ^:dynamic *all-features*
  (->> [single-word-features two-words-features three-words-features
        distance-features valency-features unigram-features third-order-features]
       (reduce into [])))

(defn get-fv [configuration]
  (let [raw-fv (->> *all-features*
                    (map (fn [feature-fn]
                           (struct feature
                                   (-> feature-fn meta :name)
                                   (feature-fn configuration))))
                    (filter (fn [fv] (not (nil? (:str fv))))))]
    (->> raw-fv
         (map (fn [f] (str (:type f) "-and-" (:str f))))
         (map feature-to-id)
         (map #(vector % 1))
         (vec))))

(defn generate-gold [sentence]
  (let [init-config (config/make-Configuration sentence)
        actions (action/get-gold-actions init-config)]
    (loop [config init-config
           actions-idx 0
           result []]
      (if (= actions-idx (count actions))
        result
        (let [current-action (nth actions actions-idx)
              next-config ((action/action-mapping current-action) config)]
          (recur
           next-config
           (inc actions-idx)
           (conj result [(action/action2id current-action)
                         (get-fv config)])))))))
