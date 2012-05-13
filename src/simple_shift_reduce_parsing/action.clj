(ns simple_shift_reduce_parsing.action)

(defn safe-index [idx sentence-length]
  (if (> sentence-length idx) idx 1))

(defn shift [[idx sentence]]
  [(safe-index (inc idx) (count sentence))
   sentence])

;; [[a] [b] [c] [d]]でbとcの間でright => [[a] [b [c]] [d]]
(defn right [[idx sentence]]
  (let [[lhs rhs] (map vec (split-at idx sentence))
	left-word (last lhs)
	right-word (first rhs)]
    [(safe-index idx (dec (count sentence)))
     (vec (concat (vec (butlast lhs))
		  (concat [(update-in left-word
				      [:children]
				      conj
				      right-word)]
			  (vec (drop 1 rhs)))))]))

;; [[a] [b] [c] [d]]でbとcの間でleft => [[a] [c [b]] [d]]
(defn left [[idx sentence]]
  (let [[lhs rhs] (map vec (split-at idx sentence))
	left-word (last lhs)
	right-word (first rhs)]
    [(safe-index idx (dec (count sentence)))
     (vec (concat (vec (butlast lhs))
		  (concat [(update-in right-word
				      [:children]
				      #(vec (concat [%2] %1))
				      left-word)]
			  (vec (drop 1 rhs)))))]))

(def actions #{right left shift})

(def action-mapping {:right right
		     :left left
		     :shift shift})

(defn generate-gold' [[action idx sentence]]
  (let [[idx sentence] ((get action-mapping action identity) [idx sentence])
	[lhs rhs] (map vec (split-at idx sentence))
	left-word (last lhs)
	right-word (first rhs)
	next-action (cond (= 1 (count sentence)) nil
			  (and
			   ;; 係り先の単語に係る単語が左のコンテキストに一つもいない
			   (reduce (fn [cum w] (and cum (not (= (:target-idx w) (:original-idx left-word)))))
				   true (butlast lhs))
			   (= (:target-idx left-word) (:original-idx right-word)))
			  :left
			  (and
			   ;; 係り先の単語に係る単語が右のコンテキストに一つもいない
			   (reduce (fn [cum w] (and cum (not (= (:target-idx w) (:original-idx right-word)))))
				   true (rest rhs))
			   (= (:target-idx right-word) (:original-idx left-word)))
			  :right
			  :else :shift)]
    [next-action idx sentence]))

(defn generate-gold [sentence]
  (->> (iterate generate-gold' [nil 1 sentence])
       (rest) ;; 初期は捨てる
       (take-while (fn [[action _ _]] (not (nil? action))))
       (map (fn [[action idx sentence]] {:action action
					 :index idx
					 :sentence sentence}))))
