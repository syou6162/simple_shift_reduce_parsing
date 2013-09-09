(ns simple_shift_reduce_parsing.test.feature
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:require [simple_shift_reduce_parsing.action :as action])
  (:use [simple_shift_reduce_parsing.feature])
  (:use [clojure.test]))

(import '[simple_shift_reduce_parsing.configuration Configuration])

(deftest test-feature
  (let [sentence [{:surface :root, :pos-tag :root, :idx 0, :head -1}
                  {:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}
                  {:surface "haag", :pos-tag "NNP", :idx 2, :head 3}
                  {:surface "plays", :pos-tag "VBZ", :idx 3, :head 0}
                  {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3}
                  {:surface ".", :pos-tag ".", :idx 5, :head 3}]
        configuration ((comp action/shift) (config/Configuration. [] sentence {}))]
    (are [x y] (= x y)
         (zero-minus-word-feature configuration) :root
         (zero-minus-pos-feature configuration) :root
         (zero-plus-word-feature configuration) "ms."
         (zero-plus-pos-feature configuration) "NNP")))

(deftest leftmost-dependent-of-input-pos-feature-test
  (let [sentence [{:surface :root, :pos-tag :root, :idx 0, :head -1}
                  {:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}
                  {:surface "haag", :pos-tag "NNP", :idx 2, :head 3}
                  {:surface "plays", :pos-tag "VBZ", :idx 3, :head 0}
                  {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3}
                  {:surface ".", :pos-tag ".", :idx 5, :head 3}]
        init-config ((comp action/left action/shift action/shift)
                     (config/make-Configuration sentence))]
    (is (= (leftmost-dependent-of-input-pos-feature init-config) "NNP"))))

(deftest head-of-pos-feature-test
  (let [sentence [{:surface :root, :pos-tag :root, :idx 0, :head -1}
                  {:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}
                  {:surface "haag", :pos-tag "NNP", :idx 2, :head 3}
                  {:surface "plays", :pos-tag "VBZ", :idx 3, :head 0}
                  {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3}
                  {:surface ".", :pos-tag ".", :idx 5, :head 3}]
        init-config ((comp action/right action/reduce action/shift
                           action/left action/shift action/shift)
                     (config/Configuration. [] sentence {}))]
    (is (= (head-of-stack-pos-feature init-config) :root))))

(deftest rightmost-dependent-of-stack-pos-feature-test
  (let [sentence [{:surface :root, :pos-tag :root, :idx 0, :head -1}
                  {:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}
                  {:surface "haag", :pos-tag "NNP", :idx 2, :head 3}
                  {:surface "plays", :pos-tag "VBZ", :idx 3, :head 0}
                  {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3}
                  {:surface ".", :pos-tag ".", :idx 5, :head 3}]
        config ((comp action/reduce action/right action/right
                      action/reduce action/shift action/left
                      action/shift action/shift)
                (config/make-Configuration sentence))]
    (is (= (rightmost-dependent-of-stack-pos-feature config) "NNP"))))
