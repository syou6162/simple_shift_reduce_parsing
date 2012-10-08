(ns simple_shift_reduce_parsing.action
  (:use simple_shift_reduce_parsing.feature))

(defn safe-index [idx sentence-length]
  (if (> sentence-length idx) idx 1))

(defn shift [[idx sentence]]
  [(safe-index (inc idx) (count sentence))
   sentence])

;; [[a] [b] [c] [d]]でbとcの間でright => [[a] [b [c]] [d]]
(defn right [[idx sentence]]
  (let [[lhs rhs] (map vec (split-at idx sentence))
	left-word (last lhs)
	right-word (assoc (first rhs) :head (:idx left-word))]
    [(safe-index idx (dec (count sentence)))
     (vec (concat (vec (butlast lhs))
		  (concat [(update-in left-word
				      [:modifiers]
				      conj
				      right-word)]
			  (vec (drop 1 rhs)))))]))

;; [[a] [b] [c] [d]]でbとcの間でleft => [[a] [c [b]] [d]]
(defn left [[idx sentence]]
  (let [[lhs rhs] (map vec (split-at idx sentence))
	right-word (first rhs)
        left-word (assoc (last lhs) :head (:idx right-word))]
    [(safe-index idx (dec (count sentence)))
     (vec (concat (vec (butlast lhs))
		  (concat [(update-in right-word
				      [:modifiers]
				      #(vec (concat [%2] %1))
				      left-word)]
			  (vec (drop 1 rhs)))))]))

(def actions #{right left shift})

(def action-mapping {:right right
		     :left left
		     :shift shift})

(defn generate-gold-seq-of-actions' [[action idx sentence]]
  (let [[idx sentence] ((get action-mapping action identity) [idx sentence])
	[lhs rhs] (map vec (split-at idx sentence))
	left-word (last lhs)
	right-word (first rhs)
	next-action (cond (= 1 (count sentence)) nil
			  (and
			   ;; 係り先の単語に係る単語が左のコンテキストに一つもいない
			   (reduce (fn [cum w] (and cum (not (= (:head w) (:idx left-word)))))
				   true (butlast lhs))
			   (= (:head left-word) (:idx right-word)))
			  :left
			  (and
			   ;; 係り先の単語に係る単語が右のコンテキストに一つもいない
			   (reduce (fn [cum w] (and cum (not (= (:head w) (:idx right-word)))))
				   true (rest rhs))
			   (= (:head right-word) (:idx left-word)))
			  :right
			  :else :shift)]
    [next-action idx sentence]))

(defn generate-gold-seq-of-actions [sentence]
  (->> (iterate generate-gold-seq-of-actions' [nil 1 sentence])
       (rest) ;; 初期は捨てる
       (take-while (fn [[action _ _]] (not (nil? action))))
       (map (fn [[action idx sentence]] {:action action
					 :index idx
					 :sentence sentence}))))

(defn generate-gold [sentence]
  (map (fn [chunk]
         (vector (:action chunk)
                 (get-fv (:sentence chunk) (:index chunk))))
       (generate-gold-seq-of-actions sentence)))
