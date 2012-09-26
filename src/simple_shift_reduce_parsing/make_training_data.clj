(ns simple_shift_reduce_parsing.make_training_data
  (:require [clojure.data.json :as json])
  (:use fobos_clj.fobos)
  (:use fobos_clj.svm)
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
;;   (-> (first (read-mst-format-file filename)))
;;   #_(reduce + (for [sentence (read-mst-format-file filename)]
;;                 (reduce + (map count (generate-gold sentence))))))


#_(defn -main [& args]
  (let [actions (for [sentence (read-mst-format-file filename)
                            gold (generate-gold sentence)]
                        (let [sent (:sentence gold)
                              action (:action gold)]
                          action))]
    (frequencies actions)))

(defn -main [& args]
  (let [training-data (for [sentence (read-mst-format-file filename)
                            gold (generate-gold sentence)]
                        (let [sent (:sentence gold)
                              action (:action gold)
                              idx (:index gold)
                              fv (map (fn [item]
                                        [(str (item :type) "-" (item :str)) 1.0])
                                      (get-fv sentence idx))]
                          {:action action :fv fv}))
        shift-data (map #(vector (if (= (:action %) :shift) 1 0) (:fv %)) training-data)
        left-data (map #(vector (if (= (:action %) :left) 1 0) (:fv %)) training-data)
        right-data (map #(vector (if (= (:action %) :right) 1 0) (:fv %)) training-data)
        [init-shift-model init-right-model init-left-model] (map #(fobos_clj.svm.SVM. % {} 1.0 1.0)
                                                                 [shift-data left-data right-data])]
    (loop [iter 0
           models [init-shift-model init-right-model init-left-model]]
      (let [[shift-model right-model left-model] models]
        (if (= iter 10)
          [(:weight shift-model)
           (:weight right-model)
           (:weight left-model)]
          (recur (inc iter)
                 [(update-weight shift-model iter)
                  (update-weight right-model iter)
                  (update-weight left-model iter)]))))))

;; (with-open [w (java.io.FileWriter. "test.json")]
;;       (.write w (json/json-str training-data)))