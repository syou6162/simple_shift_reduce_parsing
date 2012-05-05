(ns simple_shift_reduce_parsing.core)

(use '[clojure.string :only (split)])

(def filename "data/train.ulab")

(defn read-mst-format-file [filename]
  (->> (split (slurp filename) #"\n\n")
       (map (fn [lines]
	      (let [[words pos-tags indexes]
		    (map (fn [line]
			   (split line #"\t"))
			 (split lines #"\n"))]
		{:words words
		 :pos-tags pos-tags
		 :indexes indexes})))
       (vec)))

(defn -main [& args]
  (count (read-mst-format-file filename)))
