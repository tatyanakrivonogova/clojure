(ns lab3.task1
  (:require [lab3.common :refer :all]))

(defn parallel-filter
  [predicate num-threads coll]
  (->>
    (partition-collection num-threads coll)
    (map (fn [collection-chunk]
           (future 
             (->>
               (filter predicate collection-chunk)
               (doall)))))
    (mapcat deref))
)