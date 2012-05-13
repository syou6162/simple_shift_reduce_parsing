(ns simple_shift_reduce_parsing.core
  (:use simple_shift_reduce_parsing.feature
	simple_shift_reduce_parsing.word
	simple_shift_reduce_parsing.action))

(use '[clojure.string :only (split)])

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
