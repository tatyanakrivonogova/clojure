(ns lab2.task2
  (:require [lab2.common :refer [trapezoid]]))

(defrecord integral-iter-data
  [prev-integral-val
   index])

(defn new-integral-from-prev
  [f h]
  (fn [integral-iter-data-example]
    (let [idx (:index integral-iter-data-example)]
      (integral-iter-data.
        (+
          (:prev-integral-val integral-iter-data-example)
          (trapezoid
            f
            (* h (dec idx))
            (* h idx)))
        (inc idx)))))

(defn lazy-integral
  [f n]
  (fn [x]
    (let [h (/ x n)
          iter-f (new-integral-from-prev f h)
          seq (map
           :prev-integral-val
           (iterate
             iter-f
             (integral-iter-data. 0 1)))]
      (+
        (nth seq n)
        ;; последняя трапеция считается отдельно, 
        ;; так как ее высота в общем случае может быть не равна h
        (trapezoid f (* h n) x)))))