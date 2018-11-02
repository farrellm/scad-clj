(ns scad-clj.core-test
  (:require [clojure.test :refer :all]
            [scad-clj.model :as model]))

(deftest polymorphic-offset
  (testing "Numeric offset"
    (is (= (model/offset 2 ::subject)
           '(:offset {:r 2} ::subject))))
  (testing "Hash-map offset, radius"
    (is (= (model/offset {:r 2} ::subject)
           '(:offset {:r 2 :delta nil :chamfer nil} ::subject))))
  (testing "Hash-map offset, delta"
    (is (= (model/offset {:delta 1} ::subject)
           '(:offset {:r nil :delta 1 :chamfer nil} ::subject))))
  (testing "Hash-map offset, composite, variary"
    (is (= (model/offset {:delta 3 :chamfer true} ::subject0 ::subject1)
           '(:offset {:r nil :delta 3 :chamfer true} ::subject0 ::subject1)))))
