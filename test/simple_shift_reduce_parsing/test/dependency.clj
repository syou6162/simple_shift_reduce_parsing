(ns simple_shift_reduce_parsing.test.dependency
  (:use [simple_shift_reduce_parsing.dependency])
  (:use [clojure.test]))

(deftest dep-test
  (let [surfaces ["the" "luxury" "auto" "maker" "last"
                  "year" "sold" "1,214" "cars" "in" "the" "u.s."]
        pos-tags ["DT" "NN" "NN" "NN" "JJ" "NN" "VBD"
                  "CD" "NNS" "IN" "DT" "NNP"]
        heads [4 4 4 7 6 7 0 9 7 7 12 10]]
    (is (= (dep surfaces pos-tags heads)
           [{:surface :root, :pos :root, :head nil,
             :modifiers [{:surface "sold", :pos-tag "VBD", :idx 7, :head 0,
                          :modifiers [{:surface "maker", :pos-tag "NN", :idx 4, :head 7,
                                       :modifiers [{:surface "the", :pos-tag "DT", :idx 1, :head 4, :modifiers []}
                                                   {:surface "luxury", :pos-tag "NN", :idx 2, :head 4, :modifiers []}
                                                   {:surface "auto", :pos-tag "NN", :idx 3, :head 4, :modifiers []}]}
                                      {:surface "year", :pos-tag "NN", :idx 6, :head 7,
                                       :modifiers [{:surface "last", :pos-tag "JJ", :idx 5, :head 6, :modifiers []}]}
                                      {:surface "cars", :pos-tag "NNS", :idx 9, :head 7,
                                       :modifiers [{:surface "1,214", :pos-tag "CD", :idx 8, :head 9, :modifiers []}]}
                                      {:surface "in", :pos-tag "IN", :idx 10, :head 7,
                                       :modifiers [{:surface "u.s.", :pos-tag "NNP", :idx 12, :head 10,
                                                    :modifiers [{:surface "the", :pos-tag "DT", :idx 11, :head 12, :modifiers []}]}]}]}]}]))))
