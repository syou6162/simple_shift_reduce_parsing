(ns simple_shift_reduce_parsing.feature
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:require [simple_shift_reduce_parsing.action :as action])
  (:use clojure.set))

(import '[simple_shift_reduce_parsing.configuration Configuration])

(defstruct feature :type :str)
(def feature-names (atom []))

(defn get-stack-idx [stack idx]
  (let [n (count stack)]
    (- n (- idx))))

(defmacro def-feature-fn [feature-name args & body]
  `(let [name# (defn ~feature-name ~args ~@body)]
     (swap! feature-names conj name#)))

(defmacro def-around-feature-fn [feature-name idx type]
  `(def-feature-fn ~feature-name [configuration#]
     (struct
      feature
      '~feature-name
      (if (neg? ~idx)
        (let [stack# (get configuration# :stack)]
          (get-in stack# [(get-stack-idx stack# ~idx) ~type]))
        (get-in configuration# [:input ~idx ~type])))))

; (def-around-feature-fn two-minus-word-feature -3 :surface)
(def-around-feature-fn two-minus-pos-feature -3 :pos-tag)

; (def-around-feature-fn one-minus-word-feature -2 :surface)
(def-around-feature-fn one-minus-pos-feature -2 :pos-tag)

(def-around-feature-fn zero-minus-word-feature -1 :surface)
(def-around-feature-fn zero-minus-pos-feature -1 :pos-tag)

(def-around-feature-fn zero-plus-word-feature 0 :surface)
(def-around-feature-fn zero-plus-pos-feature 0 :pos-tag)

; (def-around-feature-fn one-plus-word-feature 1 :surface)
(def-around-feature-fn one-plus-pos-feature 1 :pos-tag)

; (def-around-feature-fn two-plus-word-feature 2 :surface)
(def-around-feature-fn two-plus-pos-feature 2 :pos-tag)

; (def-around-feature-fn three-plus-word-feature 3 :surface)
(def-around-feature-fn three-plus-pos-feature 3 :pos-tag)

;; NL.POS feature
(def-feature-fn leftmost-dependent-of-input-pos-feature
  [{stack :stack
    input :input
    relations :relations
    :as config}]
  (let [n (first input) ; next input token
        nl (first (get (:head-to-modifiers relations) n))] ; leftmost dependent of n
    (struct feature
            'leftmost-dependent-of-input-pos-feature
            (:pos-tag nl))))

;; TH.POS feature
(def-feature-fn head-of-pos-feature [{stack :stack
                                      input :input
                                      relations :relations
                                      :as config}]
  (let [t (peek stack) ; top of the stack
        th (get-in relations [:modifier-to-head t])] ; head of t
    (struct feature
            'head-of-pos-feature
            (:pos-tag th))))

(def-feature-fn leftmost-dependent-of-stack-pos-feature
  [{stack :stack
    input :input
    relations :relations
    :as config}]
  (let [t (peek stack) ; top of the stack
        tl (first (get-in relations [:head-to-modifiers t]))]
    (struct feature
            'leftmost-dependent-of-stack-pos-feature
            (:pos-tag tl))))

(def-feature-fn rightmost-dependent-of-stack-pos-feature
  [{stack :stack
    input :input
    relations :relations
    :as config}]
  (let [t (peek stack) ; top of the stack
        tr (peek (get-in relations [:head-to-modifiers t]))]
    (struct feature
            'rightmost-dependent-of-stack-pos-feature
            (:pos-tag tr))))

(def-feature-fn prev-action-feature [config]
  (struct feature
          'prev-action-feature
          (first (:prev-actions (meta config)))))

(def-feature-fn preprev-action-feature [config]
  (struct feature
          'preprev-action-feature
          (second (:prev-actions (meta config)))))

(defn third [coll] ((comp first next next) coll))

(def-feature-fn prepreprev-action-feature [config]
  (struct feature
          'prepreprev-action-feature
          (third (:prev-actions (meta config)))))

(defn feature-bigram [f1 f2]
  (let [type1 (:type f1)
        str1 (:str f1)
        type2 (:type f2)
        str2 (:str f2)]
    (struct feature
            (symbol (str type1 "-and-" type2))
            (str str1 "-and-" str2))))

(defn combinational-features [raw-fv]
  (let [n (count raw-fv)
        fv-bigram (for [i (range n)
                        j (range n)
                        :when (< i j)]
                    (feature-bigram (nth raw-fv i) (nth raw-fv j)))]
    (vec (concat raw-fv fv-bigram))))

(defn get-fv [configuration]
  (let [raw-fv (->> (seq @feature-names)
                    (map (fn [feature-fn] (feature-fn configuration)))
                    (flatten)
                    (filter (fn [fv] (not (nil? (:str fv))))))]
    (->> (combinational-features raw-fv)
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
           (conj result [current-action (get-fv config)])))))))
