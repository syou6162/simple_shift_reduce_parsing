(ns simple_shift_reduce_parsing.test.core
  (:use [simple_shift_reduce_parsing.dependency])
  (:use [simple_shift_reduce_parsing.core])
  (:use [clojure.test]))

(deftest extract-surfaces-test
  (let [surfaces ["the" "luxury" "auto" "maker" "last"
                  "year" "sold" "1,214" "cars" "in" "the" "u.s."]
        pos-tags ["DT" "NN" "NN" "NN" "JJ" "NN" "VBD"
                  "CD" "NNS" "IN" "DT" "NNP"]
        heads [4 4 4 7 6 7 0 9 7 7 12 10]]
    (is (= (-> (dep surfaces pos-tags heads)
               (extract-surfaces))
           [[:root ["sold" ["maker" ["the"] ["luxury"] ["auto"]]
                    ["year" ["last"]] ["cars" ["1,214"]]
                    ["in" ["u.s." ["the"]]]]]]))))
