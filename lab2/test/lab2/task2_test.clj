(ns lab2.task2-test
  (:require [clojure.test :refer :all]
            [lab2.task2 :refer :all]
            [lab2.common :refer :all]
            [lab2.common-test :refer [approx=]]))

;; (deftest test-integral-iter-data
;;   (testing "Создание record integral-iter-data"
;;     (let [data (integral-iter-data. 10.5 3)]
;;       (is (= 10.5 (:prev-integral-val data)))
;;       (is (= 3 (:index data))))))

;; (deftest test-new-integral-from-prev
;;   (testing "Функция итерации вычисляет следующий шаг"
;;     (let [iter-fn (new-integral-from-prev linear-fn 1.0)
;;           initial (integral-iter-data. 0 1)
;;           first-step (iter-fn initial)
;;           second-step (iter-fn first-step)]
;;       (is (approx= 0.5 (:prev-integral-val first-step) 0.001))
;;       (is (= 2 (:index first-step)))
;;       (is (approx= 2.0 (:prev-integral-val second-step) 0.001)) ; ∫x dx от 0 до 2 = 2
;;       (is (= 3 (:index second-step))))))

(deftest test-lazy-integral
  (testing "Ленивое интегрирование линейной функции"
    (let [integral (lazy-integral linear-fn 100)]
      (is (approx= 2.0 (integral 2) 0.01))
      (is (approx= 0.5 (integral 1) 0.01))
      (is (approx= 8.0 (integral 4) 0.01))))
  
  (testing "Ленивое интегрирование квадратичной функции"
    (let [integral (lazy-integral square-fn 1000)]
      (is (approx= 0.333 (integral 1) 0.01))
      (is (approx= 2.666 (integral 2) 0.01))
      (is (approx= 9.0 (integral 3) 0.1))))
)