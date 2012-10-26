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
         (zero-minus-word-feature configuration)
         {:type 'zero-minus-word-feature, :str :root}

         (zero-minus-pos-feature configuration)
         {:type 'zero-minus-pos-feature, :str :root}

         (zero-plus-word-feature configuration)
         {:type 'zero-plus-word-feature, :str "ms."}

         (zero-plus-pos-feature configuration)
         {:type 'zero-plus-pos-feature, :str "NNP"})))

(deftest leftmost-dependent-of-input-pos-feature-test
  (let [sentence [{:surface :root, :pos-tag :root, :idx 0, :head -1, :modifiers []}
                  {:surface "ms.", :pos-tag "NNP", :idx 1, :head 2, :modifiers []}
                  {:surface "haag", :pos-tag "NNP", :idx 2, :head 3, :modifiers []}
                  {:surface "plays", :pos-tag "VBZ", :idx 3, :head 0, :modifiers []}
                  {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3, :modifiers []}
                  {:surface ".", :pos-tag ".", :idx 5, :head 3, :modifiers []}]
        init-config ((comp action/left action/shift action/shift)
                     (config/make-Configuration sentence))]
    (is (= (leftmost-dependent-of-input-pos-feature init-config)
           {:type 'leftmost-dependent-of-input-pos-feature, :str "NNP"}))))

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
    (is (= (head-of-pos-feature init-config)
           {:type 'head-of-pos-feature, :str :root}))))

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
    (is (= (rightmost-dependent-of-stack-pos-feature config)
           {:type 'rightmost-dependent-of-stack-pos-feature, :str "NNP"}))))

(deftest feature-bigram-test
  (is (= (feature-bigram (struct feature 'hoge :fuga)
                         (struct feature 'hoge :fuga))
         {:type 'hoge-and-hoge, :str ":fuga-and-:fuga"})))

(deftest combinational-features-test
  (is (= (combinational-features [(struct feature 'hoge1 :hoge1)
                                  (struct feature 'hoge2 :hoge2)])
         [{:type 'hoge1, :str :hoge1}
          {:type 'hoge2, :str :hoge2}
          {:type 'hoge1-and-hoge2, :str ":hoge1-and-:hoge2"}])))
