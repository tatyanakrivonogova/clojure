(ns lab2.task2
  (:require [lab2.common :refer [trapezoid]]))

(defrecord integral-iter-data
  [prev-integral-val
   index])

(defn new-integral-from-prev
  [f h]
  (fn [integral-iter-data-example]
    (let [idx (:index integral-iter-data-example)
          a (* h (dec idx))
          b (* h idx)]
      (integral-iter-data.
        (+ (:prev-integral-val integral-iter-data-example)
           (trapezoid f a b))
        (inc idx)))))

(defn lazy-integral
  [f n]
  (
    fn [x]
    (if (<= x 0)
      0
      (let [h (/ x n)
            iter-f (new-integral-from-prev f h)
            seq (map :prev-integral-val
              (iterate
                iter-f
                (integral-iter-data. 0 1)))]
      (nth seq n))
    )))