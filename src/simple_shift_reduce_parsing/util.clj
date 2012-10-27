(ns simple_shift_reduce_parsing.util
  (:use [dorothy.core]))

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
