(ns simple_shift_reduce_parsing.evaluation)

(defn get-dependency-accuracy [original-sentences parsed-sentences]
  "original-sentences: flat sentences
   parsed-sentences: sentences with hierarchical structure"
  (let [original-sentences (rest original-sentences) ;; remove root
        parsed-sentences (rest parsed-sentences) ;; remove root
        pairs (map (fn [[original-sentence parsed-sentence]]
                     (let [num-of-correct-heads (reduce + (map (fn [[gold predict]]
                                                                 (if (= gold predict) 1.0 0.0))
                                                               (map vector
                                                                    (map :head original-sentence)
                                                                    (map :head parsed-sentence))))]
                       (vector num-of-correct-heads (count original-sentence))))
                   (map vector original-sentences parsed-sentences))
        corrects (map first pairs)
        lengths (map second pairs)]
    (/ (reduce + corrects) (reduce + lengths))))
