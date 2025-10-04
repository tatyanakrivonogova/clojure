(ns lab2.task1-test
  (:require [clojure.test :refer :all]
            [lab2.task1 :refer :all]
            [lab2.common :refer :all]
            [lab2.common-test :refer [approx=]]))

(deftest test-simple-integral
  (testing "Интеграл от линейной функции"
    (let [integral (simple-integral linear-fn 100)]
      (is (approx= 2.0 (integral 2) 0.01))
      (is (approx= 0.5 (integral 1) 0.01))
      (is (approx= 8.0 (integral 4) 0.01))))
  
  (testing "Интеграл от квадратичной функции"
    (let [integral (simple-integral square-fn 1000)]
      (is (approx= 0.333 (integral 1) 0.01))
      (is (approx= 2.666 (integral 2) 0.01))
      (is (approx= 9.0 (integral 3) 0.1))))
  
  (testing "Отрицательные и нулевые границы"
    (let [integral (simple-integral linear-fn 100)]
      (is (= 0 (integral 0)))
      (is (= 0 (integral -5))))))

(deftest test-memoized-integral
  (testing "Мемоизация работает"
    (let [integral (memoized-integral square-fn 100)
          first-call (time (integral 5))
          second-call (time (integral 5))]
      (is (= first-call second-call))
      (is (approx= 41.666 first-call 0.1))))) ; ∫x² dx от 0 до 5 = 125/3 ≈ 41.666