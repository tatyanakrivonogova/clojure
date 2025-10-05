(ns lab2.common-test
  (:require [clojure.test :refer :all]
            [lab2.common :refer :all]))

;; Вспомогательная функция для приблизительного сравнения чисел
(defn approx= [expected actual tolerance]
  (< (Math/abs (- expected actual)) tolerance))

(deftest test-trapezoid
  (testing "trapezoid for linear function"
    (is (= 0.0 (trapezoid linear-fn 0 0)))
    (is (= 0.5 (trapezoid linear-fn 0 1)))
    (is (= 2.0 (trapezoid linear-fn 0 2)))
    (is (= 10.0 (trapezoid square-fn 1 3)))
    (is (= 50.0 (trapezoid linear-fn 0 10)))
    (is (= 49.5 (trapezoid linear-fn 1 10))))
  
  (testing "trapezoid for square function"
    (is (= 0.0 (trapezoid square-fn 0 0)))
    (is (= 0.5 (trapezoid square-fn 0 1)))
    (is (= 4.0 (trapezoid square-fn 0 2)))
    (is (= 10.0 (trapezoid square-fn 1 3)))
    (is (= 500.0 (trapezoid square-fn 0 10)))
    (is (= 454.5 (trapezoid square-fn 1 10)))))

(deftest test-trapezoidal-rule
  (testing "trapezoidal-rule for linear function"
    (is (approx= 0.0 (trapezoidal-rule linear-fn 0 0 100) 0.01))
    (is (approx= 0.5 (trapezoidal-rule linear-fn 0 1 100) 0.01))
    (is (approx= 2.0 (trapezoidal-rule linear-fn 0 2 100) 0.01))
    (is (approx= 12.5 (trapezoidal-rule linear-fn 0 5 100) 0.01))
    (is (approx= 5000.0 (trapezoidal-rule linear-fn 0 100 100) 0.01)))
  
  (testing "trapezoidal-rule for square function"
    (is (approx= 0.0 (trapezoidal-rule square-fn 0 0 1000) 0.01))
    (is (approx= 0.333 (trapezoidal-rule square-fn 0 1 1000) 0.01))
    (is (approx= 2.666 (trapezoidal-rule square-fn 0 2 1000) 0.01))
    (is (approx= 41.666 (trapezoidal-rule square-fn 0 5 1000) 0.01))
    (is (approx= 333333.5 (trapezoidal-rule square-fn 0 100 1000) 0.01))))
