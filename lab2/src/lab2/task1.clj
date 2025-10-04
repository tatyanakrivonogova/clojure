(ns lab2.task1
  (:require [lab2.common :refer [trapezoidal-rule]]))

;; Подсчет интеграла функции как площади под графиком от точки 0 до точки x
(defn simple-integral [f n]
  (fn [x]
    (if (<= x 0)
      0
      (trapezoidal-rule f 0 x n))))

;; Мемоизация оператора simple-integral
(defn memoized-integral [f n]
  ((memoize simple-integral) f n))