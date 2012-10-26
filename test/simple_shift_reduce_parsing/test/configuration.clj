(ns simple_shift_reduce_parsing.test.configuration
  (:use [simple_shift_reduce_parsing.configuration])
  (:import [simple_shift_reduce_parsing.configuration Configuration])
  (:use [clojure.test]))

(deftest make-Configuration-test
  (are [x y] (= x y)
       (make-Configuration [1 2 3])
       (Configuration. [] [1 2 3] {:head-to-modifiers {}, :modifier-to-head {}})))

(deftest add-relation-test
  (is (= (-> (make-Configuration [1 2 3])
             (add-dependency-arc 1 2)
             (add-dependency-arc 1 3))
         (Configuration. [] [1 2 3] {:modifier-to-head {3 1, 2 1},
                                     :head-to-modifiers {1 [2 3]}}))))
