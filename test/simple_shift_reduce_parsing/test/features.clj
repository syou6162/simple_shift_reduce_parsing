(ns simple_shift_reduce_parsing.test.features
  (:use [clojure.test])
  (:use [simple_shift_reduce_parsing.features]))

(deftest test-feature
 (let [sentence [{:surface :root, :pos-tag :root, :idx 0, :head :root, :modifiers []}
                 {:surface "plays", :pos-tag "VBZ", :idx 3, :head 0,
                  :modifiers [{:surface "haag", :pos-tag "NNP", :idx 2, :head 3,
                               :modifiers [{:surface "ms.", :pos-tag "NNP", :idx 1, :head 2, :modifiers []}]}
                              {:surface "elianti", :pos-tag "NNP", :idx 4, :head 3, :modifiers []}]}
                 {:surface ".", :pos-tag ".", :idx 5, :head 3, :modifiers []}]]
   
   (are [x y] (= x y)
        (one-minus-word-feature sentence 2)
        {:type 'one-minus-word-feature, :str :root}

        (one-minus-pos-feature sentence 2)
        {:type 'one-minus-pos-feature, :str :root}
        
        (zero-minus-word-feature sentence 2)
        {:type 'zero-minus-word-feature, :str "plays"}
        
        (zero-minus-pos-feature sentence 2)
        {:type 'zero-minus-pos-feature, :str "VBZ"}
        
        (zero-plus-word-feature sentence 2)
        {:type 'zero-plus-word-feature, :str "."}
        
        (zero-plus-pos-feature sentence 2)
        {:type 'zero-plus-pos-feature, :str "."}

        (one-plus-word-feature sentence 2)
        {:type 'one-plus-word-feature, :str nil}
        
        (one-plus-pos-feature sentence 2)
        {:type 'one-plus-pos-feature, :str nil})))
