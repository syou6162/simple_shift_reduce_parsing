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

(deftest test-generate-gold
  (let [sentence [{:surface :root, :pos-tag :root, :idx 0, :head nil :modifiers []}
                  {:surface "the", :pos-tag "DT", :idx 1, :head 4 :modifiers []}
                  {:surface "luxury", :pos-tag "NN", :idx 2, :head 4 :modifiers []}
                  {:surface "auto", :pos-tag "NN", :idx 3, :head 4 :modifiers []}
                  {:surface "maker", :pos-tag "NN", :idx 4, :head 7 :modifiers []}
                  {:surface "last", :pos-tag "JJ", :idx 5, :head 6 :modifiers []}
                  {:surface "year", :pos-tag "NN", :idx 6, :head 7 :modifiers []}
                  {:surface "sold", :pos-tag "VBD", :idx 7, :head 0 :modifiers []}
                  {:surface "1,214", :pos-tag "CD", :idx 8, :head 9 :modifiers []}
                  {:surface "cars", :pos-tag "NNS", :idx 9, :head 7 :modifiers []}
                  {:surface "in", :pos-tag "IN", :idx 10, :head 7 :modifiers []}
                  {:surface "the", :pos-tag "DT", :idx 11, :head 12 :modifiers []}
                  {:surface "u.s.", :pos-tag "NNP", :idx 12, :head 10 :modifiers []}]]
    (is (= '(:shift :shift :shift :left :shift :left :left :shift
                    :left :shift :shift :left :shift :shift :left
                    :shift :right :shift :right :shift :left :left :right :right)
           (map :action (generate-gold sentence))))))