(ns simple_shift_reduce_parsing.core
  (:use simple_shift_reduce_parsing.feature
	simple_shift_reduce_parsing.word))

(use '[clojure.string :only (split)])

(def filename "data/train.ulab")
(def Root :root)

(defn read-mst-format-file [filename]
  (->> (split (slurp filename) #"\n\n")
       (map (fn [lines]
	      (let [[words pos-tags indexes]
		    (map (fn [line]
			   (split line #"\t"))
			 (split lines #"\n"))]
		(vec (map (fn [w pos-tag idx]
			    (struct word w pos-tag idx))
			  (vec (cons Root words))
			  (vec (cons Root pos-tags))
			  (vec (cons Root indexes)))))))
       (vec)))

(defn get-fv [sentence idx]
  (let [feature-fns (vals (ns-interns
			   'simple_shift_reduce_parsing.features))]
    (->> feature-fns
	 (map (fn [feature-fn] (feature-fn sentence idx)))
	 (vec))))

(defn -main [& args]
  (count (for [sentence (read-mst-format-file filename)
	       idx (range 1 (count sentence))]
	   (get-fv sentence idx))))
