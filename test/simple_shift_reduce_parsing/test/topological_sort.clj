(ns simple_shift_reduce_parsing.test.topological_sort
  (:use [simple_shift_reduce_parsing.topological_sort])
  (:use [clojure.test]))

(deftest kahn-sort-test
  (let [acyclic-g {7 #{11 8}
                   5 #{11}
                   3 #{8 10}
                   11 #{2 9}
                   8 #{9}}]
    (is (= (kahn-sort acyclic-g) [3 5 7 8 10 11 2 9]))))
