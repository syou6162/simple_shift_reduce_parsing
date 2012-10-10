(ns simple_shift_reduce_parsing.core
  (:use simple_shift_reduce_parsing.feature
	simple_shift_reduce_parsing.word
	simple_shift_reduce_parsing.action
        simple_shift_reduce_parsing.topological_sort)
  (:use [fobos-multiclass-clj.multiclass
         :only (multiclass-examples argmax-label get-models)]))

(use '[clojure.string :only (split)])
(use '[vijual :only (draw-tree)])

(defn extract-surfaces [sentence]
  (if (not (vector? sentence))
    (if (empty? (:modifiers sentence))
      [(:surface sentence)]
      (apply conj [(:surface sentence)] (map extract-surfaces (:modifiers sentence))))
    (vec (map extract-surfaces sentence))))

(defn extract-heads' [sentence result]
  (if (not (vector? sentence))
    (if (empty? (:modifiers sentence))
      [{:head (:head sentence) :idx (:idx sentence)}]
      (vec (apply concat (conj result {:head (:head sentence) :idx (:idx sentence)})
                  (map #(extract-heads' % result) (:modifiers sentence)))))
    (vec (map #(extract-heads' % result) sentence))))

(defn extract-heads [sentence]
  (map :head (sort-by :idx (first (extract-heads' sentence [])))))

; (def filename "data/train.ulab")
(def filename "data/peen.ulab")
(def Root :root)

(defn read-mst-format-file [filename]
  "Read correct parsed sentences from mst formal file.
   File format is as follows:

   ms.     haag    plays   elianti .
   NNP     NNP     VBZ     NNP     .
   2       3       0       3       3

   he      has     n't     been    able    to      replace the     m'bow   cabal   .
   PRP     VBZ     RB      VBN     JJ      TO      VB      DT      NNP     NN      .
   2       0       2       2       4       7       5       10      10      7       2"
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

(defn parse [models sentence]
  "Return parsed sentence. If the number of 'shift' action
   in the last seq-of-actions exceeds the nuber of words
   in the sentence, return nil (This means parse failed)."
  (let [init-state [1 sentence]
        sent-length (count sentence)]
    (loop [state init-state
           seq-of-actions []]
      (let [[idx sent] state]
        (cond
         (= (count sent) 1) sent

         (and (>= (count seq-of-actions) sent-length)
              (every? (partial = :shift) (take-last (inc sent-length) seq-of-actions)))
         nil

         :else (let [action (argmax-label models (get-fv sent idx))]
                 (recur
                  ((action-mapping action) [idx sent])
                  (conj seq-of-actions action))))))))

(defn -main [& args]
  (let [examples (for [sentence (read-mst-format-file filename)] sentence)
        training-examples (multiclass-examples
                           (for [sentence (read-mst-format-file filename)
                                 gold (generate-gold sentence)]
                             gold))
        iter 10
        eta 1.0
        lambda 1.0
        models (get-models training-examples iter eta lambda)]
    (dorun (map (partial parse models) examples))))
