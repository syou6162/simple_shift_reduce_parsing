(ns simple_shift_reduce_parsing.test.action
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:require [simple_shift_reduce_parsing.action :as action])
  (:use [clojure.test]))

(import '[simple_shift_reduce_parsing.configuration Configuration])

(def sentence [{:surface :root, :pos-tag :root, :idx 0, :head -1}
               {:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}
               {:surface "haag", :pos-tag "NNP", :idx 2, :head 3}
               {:surface "plays", :pos-tag "VBZ", :idx 3, :head 0}
               {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3}
               {:surface ".", :pos-tag ".", :idx 5, :head 3}])

(deftest shift-test
  (are [x y] (= x y)
       (action/shift (config/make-Configuration sentence))
       (config/Configuration. [{:surface :root, :pos-tag :root, :idx 0, :head -1}]
                              [{:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}
                               {:surface "haag", :pos-tag "NNP", :idx 2, :head 3}
                               {:surface "plays", :pos-tag "VBZ", :idx 3, :head 0}
                               {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3}
                               {:surface ".", :pos-tag ".", :idx 5, :head 3}]
                              {:head-to-modifiers {}
                               :modifier-to-head {}})))

(deftest reducable?-test
  (let [c (config/Configuration. [{:surface :root, :pos-tag :root, :idx 0, :head -1}]
                                 [{:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}
                                  {:surface "haag", :pos-tag "NNP", :idx 2, :head 3}
                                  {:surface "plays", :pos-tag "VBZ", :idx 3, :head 0}
                                  {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3}
                                  {:surface ".", :pos-tag ".", :idx 5, :head 3}]
                                 {:modifier-to-head
                                  {{:surface :root, :pos-tag :root, :idx 0, :head -1}
                                   {:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}}
                                  :head-to-modifiers {}})]
    (are [x y] (= x y)
         (action/reducable? c) true)))

(deftest reduce-test
  (let [c (config/Configuration. [{:surface :root, :pos-tag :root, :idx 0, :head -1}]
                                 [{:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}
                                  {:surface "haag", :pos-tag "NNP", :idx 2, :head 3}
                                  {:surface "plays", :pos-tag "VBZ", :idx 3, :head 0}
                                  {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3}
                                  {:surface ".", :pos-tag ".", :idx 5, :head 3}]
                                 {:modifier-to-head
                                  {{:surface :root, :pos-tag :root, :idx 0, :head -1}
                                   {:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}}
                                  :head-to-modifiers
                                  {{:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}
                                   [{:surface :root, :pos-tag :root, :idx 0, :head -1}]}})]
    (are [x y] (= x y)
         ;; reducable case
         (action/reduce c)
         (config/Configuration. []
                                [{:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}
                                 {:surface "haag", :pos-tag "NNP", :idx 2, :head 3}
                                 {:surface "plays", :pos-tag "VBZ", :idx 3, :head 0}
                                 {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3}
                                 {:surface ".", :pos-tag ".", :idx 5, :head 3}]
                                {:modifier-to-head
                                 {{:surface :root, :pos-tag :root, :idx 0, :head -1}
                                  {:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}}
                                 :head-to-modifiers
                                 {{:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}
                                  [{:surface :root, :pos-tag :root, :idx 0, :head -1}]}})
         
         ;; unreducable case => instead of reduce, do shift operation
         (action/reduce (config/Configuration. [] sentence {}))
         (config/Configuration. [{:surface :root, :pos-tag :root, :idx 0, :head -1}]
                                [{:surface "ms.", :pos-tag "NNP", :idx 1, :head 2}
                                 {:surface "haag", :pos-tag "NNP", :idx 2, :head 3}
                                 {:surface "plays", :pos-tag "VBZ", :idx 3, :head 0}
                                 {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3}
                                 {:surface ".", :pos-tag ".", :idx 5, :head 3}]
                                {}))))

(deftest get-gold-actions-test
  (is (= (action/get-gold-actions
          (config/make-Configuration sentence))
         [:shift :shift :left :shift :reduce :right :right :reduce :right])))
