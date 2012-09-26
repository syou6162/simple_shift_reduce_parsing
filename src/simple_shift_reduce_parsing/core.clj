(ns simple_shift_reduce_parsing.core
  (:use simple_shift_reduce_parsing.feature
	simple_shift_reduce_parsing.word
	simple_shift_reduce_parsing.action
        simple_shift_reduce_parsing.topological_sort))

(use '[clojure.string :only (split)])

(defn extract-surfaces [sentence]
  (if (not (vector? sentence))
    (if (empty? (:modifiers sentence))
      [(:surface sentence)]
      (apply conj [(:surface sentence)] (map extract-surfaces (:modifiers sentence))))
    (vec (map extract-surfaces sentence))))

(def filename "data/train.ulab")
(def Root :root)

(defn read-mst-format-file [filename]
  (->> (split (slurp filename) #"\n\n")
       (map (fn [lines]
	      (let [[words pos-tags heads]
		    (map (fn [line]
			   (split line #"\t"))
			 (split lines #"\n"))]
		(vec (map (fn [w pos-tag idx head]
			    (struct word w pos-tag
				    idx head[]))
			  (vec (cons Root words))
			  (vec (cons Root pos-tags))
			  (vec (range (inc (count words))))
			  (vec (cons Root (map
					   #(Integer/parseInt %)
					   heads))))))))
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
  (->> (for [sentence (read-mst-format-file filename)
	     gold (generate-gold sentence)]
	 (let [sent (:sentence gold)
	       action (:action gold)
	       idx (:index gold)]
	   (get-fv sentence idx)))
       (flatten)
       (group-by :type)
       (map (fn [[k v]] [k (count (set v))]))
       (flatten)
       (apply hash-map)))
