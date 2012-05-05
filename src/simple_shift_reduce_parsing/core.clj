(ns simple_shift_reduce_parsing.core)

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
		{:words (vec (cons Root words))
		 :pos-tags (vec (cons Root pos-tags))
		 :indexes (vec (cons Root indexes))})))
       (vec)))

;; 取りうるactionの定義
(def Right :right)
(def Left :left)
(def Shift :shift)

(def actions #{Right Left Shift})

;; 素性の定義
(defstruct feature :type :str)

(defn zero-minus-word-feature [sentence idx]
  (struct
   feature
   "0-:word"
   (get-in sentence [:words (dec idx)])))

(defn zero-plus-word-feature [sentence idx]
  (struct
   feature
   "0+:word"
   (get-in sentence [:words idx])))

(defn zero-minus-pos-feature [sentence idx]
  (struct
   feature
   "0-:pos"
   (get-in sentence [:pos-tags (dec idx)])))

(defn zero-plus-pos-feature [sentence idx]
  (struct
   feature
   "0+:pos"
   (get-in sentence [:pos-tags idx])))

(defn get-fv [sentence idx]
  (let [feature-fns [zero-minus-word-feature
		     zero-plus-word-feature
		     zero-minus-pos-feature
		     zero-plus-pos-feature]]
    (->> feature-fns
	 (map (fn [feature-fn] (feature-fn sentence idx)))
	 (vec))))

(defn -main [& args]
  (for [sentence (read-mst-format-file filename)
      idx (range 1 (count (get-in sentence [:words])))]
    (get-fv sentence idx)))
