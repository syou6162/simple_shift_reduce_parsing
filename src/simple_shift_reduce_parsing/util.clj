(ns simple_shift_reduce_parsing.util
  (:use [clj-utils.io :only (serialize deserialize)])
  (:use [clojure.string :only (split)])
  (:use [dorothy.core])
  (:require [simple_shift_reduce_parsing.word :as word]))

(def root-surface "*root-surface*")
(def root-pos-tag "*root-pos-tag*")

(defn read-mst-format-file [filename]
  "Read correct parsed sentences from mst formal file.
   File format is as follows:

   ms.     haag    plays   elianti .
   NNP     NNP     VBZ     NNP     .
   DEP     NP-SBJ  ROOT    NP-OBJ  DEP
   2       3       0       3       3

   the     luxury  auto    maker   last    year    sold    1,214   cars    in      the     u.s.
   DT      NN      NN      NN      JJ      NN      VBD     CD      NNS     IN      DT      NNP
   DEP     DEP     DEP     NP-SBJ  DEP     NP      ROOT    DEP     NP-OBJ  PP      DEP     NP
   4       4       4       7       6       7       0       9       7       7       12      10"
  (->> (split (slurp filename) #"\n\n")
       (map (fn [lines]
	      (let [[words pos-tags labels heads]
		    (map (fn [line]
			   (map clojure.string/lower-case (split line #"\t")))
                         (split lines #"\n"))]
		(vec (map (fn [w pos-tag idx head]
                            (word/make w pos-tag idx head))
			  (vec (cons root-surface words))
			  (vec (cons root-pos-tag pos-tags))
			  (vec (range (inc (count words))))
			  (vec (cons -1 (map
                                         #(Integer/parseInt %)
                                         heads))))))))
       (vec)))

(defn save-dependency-tree [sent filename]
  (let [deps (for [word sent :when (and (not (zero? (:idx word))) ;; skip root node
                                        (not (nil? (get sent (:head word)))))] ; skip if the head of the word is nil
               (let [head-word (get sent (:head word))]
                 (vector (str (:surface head-word) "_" (:idx head-word))
                         (str (:surface word) "_" (:idx word)))))
        support-info (for [word sent]
                       (vector (str (:surface word) "_" (:idx word))
                               (hash-map :label (:surface word))))]
    (-> (vec (concat deps support-info))
        digraph
        dot
        (save! filename {:format :pdf}))))

(defn print-mst-format-file [sentences]
  (doseq [sent sentences]
    (let [sent (rest sent)
          surfaces (map :surface sent)
          pos-tags (map :pos-tag sent)
          heads (map (fn [w] (if (:head w) (:head w) 0)) sent)]
      (do
        (println (apply str (interpose "\t" surfaces)))
        (println (apply str (interpose "\t" pos-tags)))
        (println (apply str (interpose "\t" (repeat (count sent) "_"))))
        (println (apply str (interpose "\t" heads)))
        (println)))))

(defn load-models [model-filename]
  (deserialize model-filename))
