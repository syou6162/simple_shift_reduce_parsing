(ns simple_shift_reduce_parsing.test.action
  (:use [simple_shift_reduce_parsing.action])
  (:use [clojure.test]))

(def sentence
  [{:surface :root, :pos-tag :root, :idx 0, :head :root, :modifiers []}
   {:surface "ms.", :pos-tag "NNP", :idx 1, :head 2, :modifiers []}
   {:surface "haag", :pos-tag "NNP", :idx 2, :head 3, :modifiers []}
   {:surface "plays", :pos-tag "VBZ", :idx 3, :head 0, :modifiers []}
   {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3, :modifiers []}
   {:surface ".", :pos-tag ".", :idx 5, :head 3, :modifiers []}])

(deftest test-actions
  (is (= ((comp right right right left left shift) [1 sentence])
	 [1 [{:surface :root, :pos-tag :root, :idx 0, :head :root,
	      :modifiers [{:surface "plays", :pos-tag "VBZ", :idx 3, :head 0,
                           :modifiers [{:surface "haag", :pos-tag "NNP", :idx 2, :head 3,
                                        :modifiers [{:surface "ms.", :pos-tag "NNP", :idx 1, :head 2, :modifiers []}]}
                                       {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3, :modifiers []}
                                       {:surface ".", :pos-tag ".", :idx 5, :head 3, :modifiers []}]}]}]])))