(ns simple_shift_reduce_parsing.hash-feature
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:require [simple_shift_reduce_parsing.action :as action]))
(import Jenkins)

;; 素性は以下の論文のBaseline feature templatesを参考に作成
;; labelに関する素性は未実装
;; http://www.sutd.edu.sg/cmsresource/faculty/yuezhang/acl11j.pdf

(import '[simple_shift_reduce_parsing.word Word])
(import '[simple_shift_reduce_parsing.configuration Configuration])

(def ^:dynamic *max-hash-length* 1000000)

;; remだと入力が負の場合で余りも負になるので、modを使うこと
(defn mod-feature-id [f] (mod f *max-hash-length*))

;; マクロ内でtype hintsはややこしいので陽に関数を定義しておく
(defn my-str-hash [^String s] (mod-feature-id (Jenkins/hashString s)))

(defn safe-stack-nth [^clojure.lang.PersistentVector stack idx]
  (letfn [(safe-pop [^clojure.lang.PersistentVector stack]
            (if-not (zero? (count stack))
              (pop stack)))]
    (cond (= idx -1) (peek stack)
          (= idx -2) (some-> stack safe-pop peek)
          (= idx -3) (some-> stack safe-pop safe-pop peek)
          (= idx -4) (some-> stack safe-pop safe-pop safe-pop peek))))

(defn safe-queue-nth [^clojure.lang.PersistentQueue coll idx]
  (letfn [(safe-pop [^clojure.lang.PersistentQueue queue]
            (if-not (zero? (count queue))
              (pop queue)))]
    (cond (= idx 0) (peek coll)
          (= idx 1) (some-> coll safe-pop peek)
          (= idx 2) (some-> coll safe-pop safe-pop peek)
          (= idx 3) (some-> coll safe-pop safe-pop safe-pop peek))))

(defmacro def-hash-feature-fn [fname ^Configuration config & body]
  (let [id (my-str-hash (str fname))
        fname' (symbol (str fname "'"))]
    `(do
       (defn ~fname' [~config] (do ~@body))
       (defn ~fname [~config]
         (if-let [result# (~fname' ~config)]
           (->> (Jenkins/hashPair ~id result#)
                (mod-feature-id)
                (int))
           (->> (Jenkins/hashPair ~id 0)
                (mod-feature-id)
                (int)))))))

(defmacro def-around-feature-fn [feature-name idx type]
  (let [config (gensym)]
    (if (neg? idx)
      `(def-hash-feature-fn ~feature-name
         ~(with-meta config {:tag 'Configuration})
         (let [stack# (.stack ~config)]
           (if-let [w-idx# (safe-stack-nth stack# ~idx)]
             (let [^Word w# (nth (.sentence ~config) w-idx#)]
               (~type w#)))))
      `(def-hash-feature-fn ~feature-name
         ~(with-meta config {:tag 'Configuration})
         (if-let [w-idx# (safe-queue-nth (.input ~config) ~idx)]
           (let [^Word w# (nth (.sentence ~config) w-idx#)]
             (~type w#)))))))

(defmacro def-conjunctive-feature-fn
  ([a b]
     (let [feature-name (symbol (str a "-and-" b))]
       `(defn ~feature-name [config#]
          (->> (Jenkins/hashPair (~a config#) (~b config#))
               (mod-feature-id)
               (int)))))
  ([a b c]
     (let [feature-name (symbol (str a "-and-" b "-and-" c))]
       `(defn ~feature-name [config#]
          (->> (Jenkins/hashTuple (~a config#) (~b config#) (~c config#))
               (mod-feature-id)
               (int)))))
  ([a b c d]
     (let [feature-name (symbol (str a "-and-" b "-and-" c "-and-" d))]
       `(defn ~feature-name [config#]
          (->> (Jenkins/hashQuadruplet
                (~a config#) (~b config#) (~c config#) (~d config#))
               (mod-feature-id)
               (int))))))

(def single-word-features
  [(def-around-feature-fn zero-minus-word-feature -1 .surface-id)
   (def-around-feature-fn zero-minus-lemma-feature -1 .lemma-id)
   (def-around-feature-fn zero-minus-pos-feature -1 .pos-tag-id)
   (def-around-feature-fn zero-minus-cpos-feature -1 .cpos-tag-id)
   (def-conjunctive-feature-fn zero-minus-word-feature zero-minus-pos-feature)
   (def-conjunctive-feature-fn zero-minus-word-feature zero-minus-cpos-feature)
   (def-conjunctive-feature-fn zero-minus-lemma-feature zero-minus-pos-feature)
   (def-conjunctive-feature-fn zero-minus-lemma-feature zero-minus-cpos-feature)

   (def-around-feature-fn zero-plus-word-feature 0 .surface-id)
   (def-around-feature-fn zero-plus-lemma-feature 0 .lemma-id)
   (def-around-feature-fn zero-plus-pos-feature 0 .pos-tag-id)
   (def-around-feature-fn zero-plus-cpos-feature 0 .cpos-tag-id)
   (def-conjunctive-feature-fn zero-plus-word-feature zero-plus-pos-feature)
   (def-conjunctive-feature-fn zero-plus-word-feature zero-plus-cpos-feature)
   (def-conjunctive-feature-fn zero-plus-lemma-feature zero-plus-pos-feature)
   (def-conjunctive-feature-fn zero-plus-lemma-feature zero-plus-cpos-feature)

   (def-around-feature-fn one-plus-word-feature 1 .surface-id)
   (def-around-feature-fn one-plus-lemma-feature 1 .lemma-id)
   (def-around-feature-fn one-plus-pos-feature 1 .pos-tag-id)
   (def-around-feature-fn one-plus-cpos-feature 1 .cpos-tag-id)
   (def-conjunctive-feature-fn one-plus-word-feature one-plus-pos-feature)
   (def-conjunctive-feature-fn one-plus-word-feature one-plus-cpos-feature)
   (def-conjunctive-feature-fn one-plus-lemma-feature one-plus-pos-feature)
   (def-conjunctive-feature-fn one-plus-lemma-feature one-plus-cpos-feature)

   (def-around-feature-fn two-plus-word-feature 2 .surface-id)
   (def-around-feature-fn two-plus-lemma-feature 2 .lemma-id)
   (def-around-feature-fn two-plus-pos-feature 2 .pos-tag-id)
   (def-around-feature-fn two-plus-cpos-feature 2 .cpos-tag-id)
   (def-conjunctive-feature-fn two-plus-word-feature two-plus-pos-feature)
   (def-conjunctive-feature-fn two-plus-word-feature two-plus-cpos-feature)
   (def-conjunctive-feature-fn two-plus-lemma-feature two-plus-pos-feature)
   (def-conjunctive-feature-fn two-plus-lemma-feature two-plus-cpos-feature)])

(def two-words-fine-features
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

(def two-words-coarse-features
  [(def-conjunctive-feature-fn
     zero-minus-lemma-feature zero-minus-cpos-feature
     zero-plus-lemma-feature zero-plus-cpos-feature)

   (def-conjunctive-feature-fn
     zero-minus-lemma-feature zero-minus-cpos-feature
     zero-plus-lemma-feature)

   (def-conjunctive-feature-fn
     zero-minus-lemma-feature
     zero-plus-lemma-feature zero-plus-cpos-feature)

   (def-conjunctive-feature-fn
     zero-minus-lemma-feature zero-minus-cpos-feature
     zero-plus-cpos-feature)

   (def-conjunctive-feature-fn
     zero-minus-cpos-feature
     zero-plus-lemma-feature zero-plus-cpos-feature)

   (def-conjunctive-feature-fn
     zero-minus-lemma-feature
     zero-plus-lemma-feature)

   (def-conjunctive-feature-fn
     zero-minus-cpos-feature
     zero-plus-cpos-feature)

   (def-conjunctive-feature-fn
     zero-plus-cpos-feature
     one-plus-cpos-feature)])

(def two-words-features
  (into two-words-fine-features two-words-coarse-features))

(defmacro def-both-word-and-pos-feature [feature-name]
  (let [word-feature-name (symbol (str feature-name "-word-feature"))
        lemma-feature-name (symbol (str feature-name "-lemma-feature"))
        pos-feature-name (symbol (str feature-name "-pos-feature"))
        cpos-feature-name (symbol (str feature-name "-cpos-feature"))]
    `(do
       (def-hash-feature-fn ~word-feature-name config#
         (-> config# ~feature-name :surface-id))
       (def-hash-feature-fn ~lemma-feature-name config#
         (-> config# ~feature-name :lemma-id))
       (def-hash-feature-fn ~pos-feature-name config#
         (-> config# ~feature-name :pos-tag-id))
       (def-hash-feature-fn ~cpos-feature-name config#
         (-> config# ~feature-name :cpos-tag-id)))))

(defn head-of-stack [^Configuration config]
  (let [t (peek (.stack config))] ; top of the stack
    (if-let [th (get-in config [:relations :modifier-to-head t])] ; head of t
      (nth (:sentence config) th))))

(defn leftmost-dependent-of-stack [^Configuration config]
  (let [t (peek (.stack config))] ; top of the stack
    (if-let [tl (first (get-in config [:relations :head-to-modifiers t]))]
      (nth (:sentence config) tl))))

(defn rightmost-dependent-of-stack [^Configuration config]
  (let [t (peek (.stack config))] ; top of the stack
    (if-let [tr (peek (get-in config [:relations :head-to-modifiers t]))]
      (nth (:sentence config) tr))))

(defn leftmost-dependent-of-input [^Configuration config]
  (let [n (peek (.input config))]  ; next input token
    (if-let [nl (first (get-in config [:relations :head-to-modifiers n]))] ; leftmost dependent of n
      (nth (:sentence config) nl))))

(def-both-word-and-pos-feature head-of-stack)
(def-both-word-and-pos-feature leftmost-dependent-of-stack)
(def-both-word-and-pos-feature rightmost-dependent-of-stack)
(def-both-word-and-pos-feature leftmost-dependent-of-input)

(def three-words-fine-features
  [(def-conjunctive-feature-fn
     zero-plus-pos-feature one-plus-pos-feature two-plus-pos-feature)

   (def-conjunctive-feature-fn
     zero-minus-pos-feature zero-plus-pos-feature one-plus-pos-feature)

   (def-conjunctive-feature-fn
     head-of-stack-pos-feature zero-minus-pos-feature zero-plus-pos-feature)

   (def-conjunctive-feature-fn
     zero-minus-pos-feature leftmost-dependent-of-stack-pos-feature zero-plus-pos-feature)

   (def-conjunctive-feature-fn
     zero-plus-pos-feature rightmost-dependent-of-stack-pos-feature zero-plus-pos-feature)

   (def-conjunctive-feature-fn
     zero-minus-pos-feature zero-plus-pos-feature leftmost-dependent-of-input-pos-feature)])

(def three-words-coarse-features
  [(def-conjunctive-feature-fn
     zero-plus-cpos-feature one-plus-cpos-feature two-plus-cpos-feature)

   (def-conjunctive-feature-fn
     zero-minus-cpos-feature zero-plus-cpos-feature one-plus-cpos-feature)

   (def-conjunctive-feature-fn
     head-of-stack-cpos-feature zero-minus-cpos-feature zero-plus-cpos-feature)

   (def-conjunctive-feature-fn
     zero-minus-cpos-feature leftmost-dependent-of-stack-cpos-feature zero-plus-cpos-feature)

   (def-conjunctive-feature-fn
     zero-plus-cpos-feature rightmost-dependent-of-stack-cpos-feature zero-plus-cpos-feature)

   (def-conjunctive-feature-fn
     zero-minus-cpos-feature zero-plus-cpos-feature leftmost-dependent-of-input-cpos-feature)])

(def three-words-features
  (into three-words-fine-features three-words-coarse-features))

(def-hash-feature-fn distance-feature ^Configuration config
  (let [i (peek (.stack config))
        j (peek (.input config))
        dist (if (and i j)
               (let [d (Math/abs (int (- i j)))]
                 (cond (> d 10) 11
                       (> d 5) 6
                       :else d))
               -1)]
    dist))

(def distance-fine-features
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

(def distance-course-features
  [(def-conjunctive-feature-fn
     zero-minus-lemma-feature distance-feature)

   (def-conjunctive-feature-fn
     zero-minus-cpos-feature distance-feature)

   (def-conjunctive-feature-fn
     zero-plus-lemma-feature distance-feature)

   (def-conjunctive-feature-fn
     zero-plus-cpos-feature distance-feature)

   (def-conjunctive-feature-fn
     zero-minus-lemma-feature zero-plus-lemma-feature distance-feature)

   (def-conjunctive-feature-fn
     zero-minus-cpos-feature zero-plus-cpos-feature distance-feature)])

(def distance-features (into distance-fine-features distance-course-features))

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

(def all-features
  (->> [single-word-features
        two-words-features
        three-words-features
        distance-features
        valency-features
        unigram-features
        third-order-features]
       (reduce into [])
       (sort-by #(-> % meta :name str count))))

(defn get-fv [^Configuration config]
  (mapv (fn [f] (f config)) all-features))
