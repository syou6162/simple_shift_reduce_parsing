(ns simple_shift_reduce_parsing.tree_zipper
  (:require [clojure.zip :as zip]))

(defn mk-zip [root]
  (let [branch? (fn [node]
                  (when node
                    (or (and (map? node)
                             (not (empty? (node :modifiers))))
                        (vector? node))))
        children (fn [node]
                   (cond
                    (nil? node) nil
                    (map? node) (node :modifiers)
                    :else node))
        make-node (fn [node children]
                    (assoc node
                      :modifiers
                      (conj (node :modifiers) children)))]
    (zip/zipper branch? children make-node root)))

(defn head-idx [node]
  (-> node
      (zip/up)
      (zip/node)
      :idx))

(defn third [lst]
  (first (rest (rest lst))))
