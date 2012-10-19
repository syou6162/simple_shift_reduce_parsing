(ns simple_shift_reduce_parsing.test.configuration
  (:require [simple_shift_reduce_parsing.configuration :as config])
  (:use [clojure.test]))

(import '[simple_shift_reduce_parsing.configuration Configuration])

(deftest get-gold-actions
  (let [sentence [{:surface :root, :pos-tag :root, :idx 0, :head -1, :modifiers []}
                  {:surface "ms.", :pos-tag "NNP", :idx 1, :head 2, :modifiers []}
                  {:surface "haag", :pos-tag "NNP", :idx 2, :head 3, :modifiers []}
                  {:surface "plays", :pos-tag "VBZ", :idx 3, :head 0, :modifiers []}
                  {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3, :modifiers []}
                  {:surface ".", :pos-tag ".", :idx 5, :head 3, :modifiers []}]]
    (is (= (config/get-gold-actions (config/Configuration. [] sentence {}))
           [:shift :shift :left :shift :reduce :right :right :reduce :right]))))
