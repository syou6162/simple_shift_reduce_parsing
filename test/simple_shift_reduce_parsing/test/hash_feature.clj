(ns simple_shift_reduce_parsing.test.hash-feature
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:require [simple_shift_reduce_parsing.action :as action])
  (:use [simple_shift_reduce_parsing.hash-feature])
  (:use [clojure.test]))

(require '[simple_shift_reduce_parsing.word :as word])
(import '[simple_shift_reduce_parsing.configuration Configuration])

(deftest test-feature
  (let [sentence [(word/make "root" "root" 0 -1)
                  (word/make "ms." "NNP" 1 2)
                  (word/make "haag" "NNP" 2 3)
                  (word/make "plays" "VBZ" 3 0)
                  (word/make "elianti" "NNP" 4 3)
                  (word/make "." "." 5 3)]
        config ((comp action/shift)
                (config/make-Configuration sentence))]
    (are [x y] (= x y)
         ; root
         (zero-minus-word-feature config) 383243
         (zero-minus-pos-feature' config) -1064259667
         ; ms.
         (zero-plus-word-feature' config) 259526741
         (zero-plus-pos-feature' config) -2026268476)))

(deftest leftmost-dependent-of-input-pos-feature-test
  (let [sentence [(word/make "root" "root" 0 -1)
                  (word/make "ms." "NNP" 1 2)
                  (word/make "haag" "NNP" 2 3)
                  (word/make "plays" "VBZ" 3 0)
                  (word/make "elianti" "NNP" 4 3)
                  (word/make "." "." 5 3)]
        init-config ((comp action/left action/shift action/shift)
                     (config/make-Configuration sentence))]
    (is (= (leftmost-dependent-of-input-pos-feature init-config)
           523410))))

(deftest head-of-pos-feature-test
  (let [sentence [(word/make "root" "root" 0 -1)
                  (word/make "ms." "NNP" 1 2)
                  (word/make "haag" "NNP" 2 3)
                  (word/make "plays" "VBZ" 3 0)
                  (word/make "elianti" "NNP" 4 3)
                  (word/make "." "." 5 3)]
        init-config ((comp action/right action/reduce action/shift
                           action/left action/shift action/shift)
                     (config/make-Configuration sentence))]
    (is (= (head-of-stack-pos-feature init-config)
           603122))))

(deftest rightmost-dependent-of-stack-pos-feature-test
  (let [sentence [(word/make "root" "root" 0 -1)
                  (word/make "ms." "NNP" 1 2)
                  (word/make "haag" "NNP" 2 3)
                  (word/make "plays" "VBZ" 3 0)
                  (word/make "elianti" "NNP" 4 3)
                  (word/make "." "." 5 3)]
        config ((comp action/reduce action/right action/right
                      action/reduce action/shift action/left
                      action/shift action/shift)
                (config/make-Configuration sentence))]
    (is (= (rightmost-dependent-of-stack-pos-feature config)
           416415))))
