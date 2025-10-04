(ns lab2.common
  (:require [clojure.math :as math]))

;; Подсчет площади одной трапеции с основаниями f(a) и f(b) и высотой (b-a)
(defn trapezoid
  [f a b]
  (*
    (+ (f a) (f b))
    (- b a)
    0.5))

;; Подсчет площади под графиком путем разбиения на n трапеций
(defn trapezoidal-rule [f a b n]
  (let [h (/ (- b a) n)
        points (map #(+ a (* % h)) (range 0 (inc n)))]
    (reduce + (map #(trapezoid f %1 %2) points (rest points)))))

;; Тестовые функции
(defn linear-fn [x] x)
(defn square-fn [x] (* x x))
(defn sin-fn [x] (Math/sin x))
(defn exp-fn [x] (Math/exp x))