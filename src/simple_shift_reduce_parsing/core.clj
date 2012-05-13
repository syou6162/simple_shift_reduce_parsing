(ns simple_shift_reduce_parsing.core
  (:use simple_shift_reduce_parsing.feature
	simple_shift_reduce_parsing.word
	simple_shift_reduce_parsing.action))

(use 'vijual)
(use '[clojure.string :only (split)])

(defn extract-surface [sentence]
  (if (not (vector? sentence))
    (if (empty? (:children sentence))
      [(keyword (:surface sentence))]
      (apply conj [(:surface sentence)] (map extract-surface (:children sentence))))
    (vec (map extract-surface sentence))))

(let [sentence [{:surface :root, :pos-tag :root, :original-idx 0, :target-idx :root,
		 :children [{:surface "sold", :pos-tag "VBD", :original-idx 7, :target-idx 0,
			     :children [{:surface "maker", :pos-tag "NN", :original-idx 4, :target-idx 7,
					 :children [{:surface "the", :pos-tag "DT", :original-idx 1, :target-idx 4, :children []}
						    {:surface "luxury", :pos-tag "NN", :original-idx 2, :target-idx 4, :children []}
						    {:surface "auto", :pos-tag "NN", :original-idx 3, :target-idx 4, :children []}]}
					{:surface "year", :pos-tag "NN", :original-idx 6, :target-idx 7,
					 :children [{:surface "last", :pos-tag "JJ", :original-idx 5, :target-idx 6, :children []}]}
					{:surface "cars", :pos-tag "NNS", :original-idx 9, :target-idx 7,
					 :children [{:surface "1,214", :pos-tag "CD", :original-idx 8, :target-idx 9, :children []}]}
					{:surface "in", :pos-tag "IN", :original-idx 10, :target-idx 7,
					 :children [{:surface "u.s.", :pos-tag "NNP", :original-idx 12, :target-idx 10,
						     :children [{:surface "the", :pos-tag "DT", :original-idx 11, :target-idx 12, :children []}]}]}]}]}]]
  (draw-tree (extract-surface sentence)))
;;                             +------+
;;                             | root |
;;                             +--+---+
;;                                |
;;                                +
;;                                |
;;                             +--+---+
;;                             | sold |
;;                             +--+---+
;;                                |
;;             +------------------+---------+--------+
;;             |                  |         |        |
;;         +---+---+           +--+---+ +---+--+   +-+--+
;;         | maker |           | year | | cars |   | in |
;;         +---+---+           +--+---+ +---+--+   +-+--+
;;             |                  |         |        |
;;    +--------+---------+        +         +        +
;;    |        |         |        |         |        |
;; +--+--+ +---+----+ +--+---+ +--+---+ +---+---+ +--+---+
;; | the | | luxury | | auto | | last | | 1,214 | | u.s. |
;; +-----+ +--------+ +------+ +------+ +-------+ +--+---+
;;                                                   |
;;                                                   +
;;                                                   |
;;                                                +--+--+
;;                                                | the |
;;                                                +-----+

(def filename "data/train.ulab")
(def Root :root)

(defn read-mst-format-file [filename]
  (->> (split (slurp filename) #"\n\n")
       (map (fn [lines]
	      (let [[words pos-tags target-indexes]
		    (map (fn [line]
			   (split line #"\t"))
			 (split lines #"\n"))]
		(vec (map (fn [w pos-tag original-idx target-idx]
			    (struct word w pos-tag
				    original-idx target-idx []))
			  (vec (cons Root words))
			  (vec (cons Root pos-tags))
			  (vec (range (inc (count words))))
			  (vec (cons Root (map
					   #(Integer/parseInt %)
					   target-indexes))))))))
       (vec)))

(defn get-fv [sentence idx]
  (let [feature-fns (vals (ns-interns
			   'simple_shift_reduce_parsing.features))]
    (->> feature-fns
	 (map (fn [feature-fn] (feature-fn sentence idx)))
	 (vec))))

;; (defn -main [& args]
;;   (reduce + (for [sentence (read-mst-format-file filename)]
;; 	      (reduce + (map count (generate-gold sentence))))))

(defn -main [& args]
  (for [sentence (read-mst-format-file filename)
	gold (generate-gold sentence)]
    (let [sent (:sentence gold)
	  action (:action gold)
	  idx (:index gold)]
      {:action action
       :fvs (get-fv sentence idx)})))
