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

(defn generate-gold' [[idx sentence]]
  (let [[lhs rhs] (map vec (split-at idx sentence))
	left-word (last lhs)
	right-word (first rhs)
	next-action (cond (= 1 (count sentence)) nil
			  (and
			   ;; 係り先の単語に係る単語が左のコンテキストに一つもいない
			   (reduce (fn [cum w] (and cum (not (= (:target-idx w) (:original-idx left-word)))))
				   true (butlast lhs))
			   (= (:target-idx left-word) (:original-idx right-word)))
			  left
			  (and
			   ;; 係り先の単語に係る単語が右のコンテキストに一つもいない
			   (reduce (fn [cum w] (and cum (not (= (:target-idx w) (:original-idx right-word)))))
				   true (rest rhs))
			   (= (:target-idx right-word) (:original-idx left-word)))
			  right
			  :else shift)]
    {:action next-action
     :index idx
     :sentence sentence}))

;; (defn generate-gold [sentence]
;;   (generate-gold' [] [1 sentence]))

(defn generate-gold [sentence]
  (loop [m (generate-gold' [1 sentence])
	 golds []]
    (if (nil? (:action m))
      golds
      (let [new-m (generate-gold' ((:action m) [(:index m) (:sentence m)]))]
	(recur new-m (conj golds new-m))))))

;(map :action (reverse (generate-gold sentence)))
					; (map :action (reverse (generate-gold sentence)))

;; (map :action (reverse (generate-gold sentence)))
;; (eval (symbol "inc"))
;; ((apply comp (map #(eval (% :action )) (reverse (generate-gold sentence)))) [1 sentence])

;; (map second (reverse (generate-gold sentence)))
