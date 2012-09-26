(ns simple_shift_reduce_parsing.dependency
  (:use [simple_shift_reduce_parsing.word])
  (:use [simple_shift_reduce_parsing.topological_sort]))

(defn dep [surfaces pos-tags heads]
  (let [indexes (range 1 (inc (count surfaces)))
        dict (zipmap indexes (map (fn [surface pos idx head]
                                    (struct-map word
                                      :surface surface
                                      :pos-tag pos
                                      :idx idx
                                      :head head
                                      :modifiers []))
                                  surfaces pos-tags indexes heads))
        topological-sort (kahn-sort (zipmap indexes (map (comp set list) heads)))]
    (->> (reduce (fn [cum idx]
                   (let [word (cum idx)]
                     (dissoc (update-in cum [(word :head) :modifiers] conj word)
                             idx)))
                 (assoc dict 0 {:surface :root, :pos :root, :head nil, :modifiers []})
                 (butlast topological-sort))
         (first)
         (second)
         (vector))))