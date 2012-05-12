(ns simple_shift_reduce_parsing.core)

(use '[clojure.string :only (split)])

(def filename "data/train.ulab")
(def Root :root)

(defstruct word :surface :pos-tag :idx)

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

;; 取りうるactionの定義
(def Right :right)
(def Left :left)
(def Shift :shift)

(def actions #{Right Left Shift})

;; 素性の定義
(defstruct feature :type :str)

(defmacro deffeature-fn
  ([feature-name idx-op type]
     `(defn ~feature-name [sentence# idx#]
	(struct
	 feature
	 '~feature-name
	 (get-in sentence# [(~idx-op idx#) ~type]))))
  ([feature-name type]
     `(deffeature-fn ~feature-name identity ~type)))

(deffeature-fn zero-minus-word-feature dec :surface)
(deffeature-fn zero-minus-pos-feature dec :pos-tag)

(deffeature-fn zero-plus-word-feature :surface)
(deffeature-fn zero-plus-pos-feature :pos-tag)

(defn get-fv [sentence idx]
  (let [feature-fns [zero-minus-word-feature
		     zero-plus-word-feature
		     zero-minus-pos-feature
		     zero-plus-pos-feature]]
    (->> feature-fns
	 (map (fn [feature-fn] (feature-fn sentence idx)))
	 (vec))))

(defn -main [& args]
  (count (for [sentence (read-mst-format-file filename)
	       idx (range 1 (count sentence))]
	   (get-fv sentence idx))))
