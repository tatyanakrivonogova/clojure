(ns lab2.common-test
  (:require [clojure.test :refer :all]
            [lab2.common :refer :all]))

;; Вспомогательная функция для приблизительного сравнения чисел
(defn approx= [expected actual tolerance]
  (< (Math/abs (- expected actual)) tolerance))

(deftest test-trapezoid
  (testing "Площадь трапеции для линейной функции"
    (is (= 2.0 (trapezoid linear-fn 0 2)))
    (is (= 0.5 (trapezoid linear-fn 0 1))))
  
  (testing "Площадь трапеции для квадратичной функции"
    (is (= 0.5 (trapezoid square-fn 0 1)))
    (is (= 10.0 (trapezoid square-fn 1 3)))))

(deftest test-trapezoidal-rule
  (testing "Интегрирование линейной функции"
    (is (approx= 2.0 (trapezoidal-rule linear-fn 0 2 100) 0.01))
    (is (approx= 0.5 (trapezoidal-rule linear-fn 0 1 100) 0.01)))
  
  (testing "Интегрирование квадратичной функции"
    (is (approx= 2.666 (trapezoidal-rule square-fn 0 2 1000) 0.01))
    (is (approx= 0.333 (trapezoidal-rule square-fn 0 1 1000) 0.01))))
