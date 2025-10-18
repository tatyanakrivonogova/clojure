(ns lab3.core
  (:require [lab3.common :refer :all])
  (:require [lab3.task1 :refer :all])
  (:require [lab3.task2 :refer :all]))

(defn benchmark []
(let [test-data (range 0 1000)
      predicate heavy-predicate]
  
  (println "filter:")
  (time (doall (filter predicate test-data)))
  
  (println "parallel-filter:")
  (println "   - parallel-filter (2 threads):")
  (time (doall (parallel-filter predicate 2 test-data)))
  (println "   - parallel-filter (4 threads):")
  (time (doall (parallel-filter predicate 4 test-data)))
  (println "   - parallel-filter (8 threads):")
  (time (doall (parallel-filter predicate 8 test-data)))
  
  (println "lazy-parallel-filter:")
  (println "   - lazy-parallel-filter (2 threads, chunk size = 8):")
  (time (doall (lazy-parallel-filter predicate 2 8 test-data)))
  (println "   - lazy-parallel-filter (2 threads, chunk size = 500):")
  (time (doall (lazy-parallel-filter predicate 2 500 test-data)))
  (println "   - lazy-parallel-filter (4 threads, chunk size = 8):")
  (time (doall (lazy-parallel-filter predicate 4 8 test-data)))
  (println "   - lazy-parallel-filter (4 threads, chunk size = 250):")
  (time (doall (lazy-parallel-filter predicate 4 250 test-data)))
  (println "   - lazy-parallel-filter (8 threads, chunk size = 8):")
  (time (doall (lazy-parallel-filter predicate 8 8 test-data)))
  (println "   - lazy-parallel-filter (8 threads, chunk size = 125):")
  (time (doall (lazy-parallel-filter predicate 8 125 test-data)))

  (println "Check lazy computations in lazy-parallel-filter")
  (println "   - take 100 (lazy-parallel-filter odd? 4 500 (iterate inc 1))")
  (time (doall (take 100 (lazy-parallel-filter odd? 4 500 (iterate inc 1)))))
  (println "   - take 200 (lazy-parallel-filter odd? 4 500 (iterate inc 1))")
  (time (doall (take 200 (lazy-parallel-filter odd? 4 500 (iterate inc 1)))))
  (println "   - take 300 (lazy-parallel-filter odd? 4 500 (iterate inc 1))")
  (time (doall (take 300 (lazy-parallel-filter odd? 4 500 (iterate inc 1)))))
  (println "   - take 400 (lazy-parallel-filter odd? 4 500 (iterate inc 1))")
  (time (doall (take 400 (lazy-parallel-filter odd? 4 500 (iterate inc 1)))))
  (println "   - take 500 (lazy-parallel-filter odd? 4 500 (iterate inc 1))")
  (time (doall (take 500 (lazy-parallel-filter odd? 4 500 (iterate inc 1)))))
))


(defn -main
  [& args]
  (let [parallel-filter-result (parallel-filter odd? 4 (range 0 100))]
    (println parallel-filter-result))

  (let [lazy-parallel-filter-result (lazy-parallel-filter odd? 4 8 (range 0 100))]
    (println lazy-parallel-filter-result))

  (let [lazy-parallel-filter-result (take 300 (lazy-parallel-filter odd? 4 8 (iterate inc 1)))]
    (println lazy-parallel-filter-result))
  
  (benchmark)
  (shutdown-agents))
