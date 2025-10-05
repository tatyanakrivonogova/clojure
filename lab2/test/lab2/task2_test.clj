(ns lab2.task2-test
  (:require [clojure.test :refer :all]
            [lab2.task2 :refer :all]
            [lab2.common :refer :all]
            [lab2.common-test :refer [approx=]]))

(deftest test-lazy-integral
  (testing "test-lazy-integral for linear function"
    (let [integral (lazy-integral linear-fn 100)]
      (is (approx= 2.0 (integral 2) 0.01))
      (is (approx= 0.5 (integral 1) 0.01))
      (is (approx= 8.0 (integral 4) 0.01))))
  
  (testing "test-lazy-integral for square function"
    (let [integral (lazy-integral square-fn 1000)]
      (is (approx= 0.333 (integral 1) 0.01))
      (is (approx= 2.666 (integral 2) 0.01))
      (is (approx= 9.0 (integral 3) 0.1))))

  (testing "corner cases"
    (let [integral (lazy-integral linear-fn 100)]
      (is (= 0 (integral 0)))
      (is (= 0 (integral -5))))))