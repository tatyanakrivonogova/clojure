(ns lab2.task1-test
  (:require [clojure.test :refer :all]
            [lab2.task1 :refer :all]
            [lab2.common :refer :all]
            [lab2.common-test :refer [approx=]]))

(deftest test-simple-integral
  (testing "test-simple-integral for linear function"
    (let [integral (simple-integral linear-fn 100)]
      (is (approx= 2.0 (integral 2) 0.01))
      (is (approx= 0.5 (integral 1) 0.01))
      (is (approx= 8.0 (integral 4) 0.01))))
  
  (testing "test-simple-integral for square function"
    (let [integral (simple-integral square-fn 1000)]
      (is (approx= 0.333 (integral 1) 0.01))
      (is (approx= 2.666 (integral 2) 0.01))
      (is (approx= 9.0 (integral 3) 0.1))))
  
  (testing "corner cases"
    (let [integral (simple-integral linear-fn 100)]
      (is (= 0 (integral 0)))
      (is (= 0 (integral -5))))))