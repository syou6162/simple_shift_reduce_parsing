(ns simple_shift_reduce_parsing.word)

(defstruct word :surface :pos-tag :idx :head :modifiers)

;; (defstruct word :surface :pos-tag
;; 	   :original-idx :target-idx
;; 	   :children)