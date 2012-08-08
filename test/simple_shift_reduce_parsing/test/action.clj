(ns simple_shift_reduce_parsing.test.action
  (:use [simple_shift_reduce_parsing.action])
  (:use [clojure.test]))

(def sentence
     [{:surface :root, :pos-tag :root, :original-idx 0, :target-idx :root, :children []}
      {:surface "ms.", :pos-tag "NNP", :original-idx 1, :target-idx 2, :children []}
      {:surface "haag", :pos-tag "NNP", :original-idx 2, :target-idx 3, :children []}
      {:surface "plays", :pos-tag "VBZ", :original-idx 3, :target-idx 0, :children []}
      {:surface "elianti", :pos-tag "NNP", :original-idx 4, :target-idx 3, :children []}
      {:surface ".", :pos-tag ".", :original-idx 5, :target-idx 3, :children []}])

(deftest test-actions
  (is (= ((comp right right right left left shift) [1 sentence])
	 [1 [{:surface :root, :pos-tag :root, :original-idx 0, :target-idx :root,
	      :children [{:surface "plays", :pos-tag "VBZ", :original-idx 3, :target-idx 0,
			  :children [{:surface "haag", :pos-tag "NNP", :original-idx 2, :target-idx 3,
				      :children [{:surface "ms.", :pos-tag "NNP", :original-idx 1, :target-idx 2, :children []}]}
				     {:surface "elianti", :pos-tag "NNP", :original-idx 4, :target-idx 3, :children []}
				     {:surface ".", :pos-tag ".", :original-idx 5, :target-idx 3, :children []}]}]}]])))

(let [sentence [{:surface :root, :pos-tag :root, :original-idx 0, :target-idx :root, :children []}
		{:surface "the", :pos-tag "DT", :original-idx 1, :target-idx 4, :children []}
		{:surface "luxury", :pos-tag "NN", :original-idx 2, :target-idx 4, :children []}
		{:surface "auto", :pos-tag "NN", :original-idx 3, :target-idx 4, :children []}
		{:surface "maker", :pos-tag "NN", :original-idx 4, :target-idx 7, :children []}
		{:surface "last", :pos-tag "JJ", :original-idx 5, :target-idx 6, :children []}
		{:surface "year", :pos-tag "NN", :original-idx 6, :target-idx 7, :children []}
		{:surface "sold", :pos-tag "VBD", :original-idx 7, :target-idx 0, :children []}
		{:surface "1,214", :pos-tag "CD", :original-idx 8, :target-idx 9, :children []}
		{:surface "cars", :pos-tag "NNS", :original-idx 9, :target-idx 7, :children []}
		{:surface "in", :pos-tag "IN", :original-idx 10, :target-idx 7, :children []}
		{:surface "the", :pos-tag "DT", :original-idx 11, :target-idx 12, :children []}
		{:surface "u.s.", :pos-tag "NNP", :original-idx 12, :target-idx 10, :children []}]]
  (generate-gold sentence))

;; [{:surface :root, :pos-tag :root, :original-idx 0, :target-idx :root, :children []}
;;  {:surface "maker", :pos-tag "NN", :original-idx 4, :target-idx 7, ;; soldにrightをしたい
;;   :children [{:surface "the", :pos-tag "DT", :original-idx 1, :target-idx 4, :children []}
;; 	     {:surface "luxury", :pos-tag "NN", :original-idx 2, :target-idx 4, :children []}
;; 	     {:surface "auto", :pos-tag "NN", :original-idx 3, :target-idx 4, :children []}]}
;;  {:surface "year", :pos-tag "NN", :original-idx 6, :target-idx 7, ;; soldにrightをしたい
;;   :children [{:surface "last", :pos-tag "JJ", :original-idx 5, :target-idx 6, :children []}]}
;;  {:surface "sold", :pos-tag "VBD", :original-idx 7, :target-idx 0, :children []}
;;  {:surface "cars", :pos-tag "NNS", :original-idx 9, :target-idx 7, ;; soldにleftをしたい
;;   :children [{:surface "1,214", :pos-tag "CD", :original-idx 8, :target-idx 9, :children []}]}
;;  {:surface "in", :pos-tag "IN", :original-idx 10, :target-idx 7,
;;   :children [{:surface "u.s.", :pos-tag "NNP", :original-idx 12, :target-idx 10,
;; 	      :children [{:surface "the", :pos-tag "DT", :original-idx 11, :target-idx 12, :children []}]}]}]