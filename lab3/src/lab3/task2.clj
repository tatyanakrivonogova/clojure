(ns lab3.task2
  (:require [lab3.common :refer :all])
  (:require [lab3.task1 :refer :all]))

(defn lazy-parallel-filter
  [predicate num-threads chunk-size coll]
  (lazy-cat
    (parallel-filter predicate num-threads (take chunk-size coll))
    (if (empty? coll)
      coll
      (lazy-parallel-filter predicate num-threads chunk-size (drop chunk-size coll)))))