(ns lab2.core
  (:require [lab2.common :refer [square-fn sin-fn exp-fn linear-fn]]
            [lab2.task1 :refer [simple-integral memoized-integral]]
            [lab2.task2 :refer [lazy-integral]]))

(defn measure-average-time [operation iterations]
  (time
      (dotimes [i iterations]
        (operation)))
)

(defn benchmark-integral-table []
  (let [test-functions [["x" linear-fn]
                        ["x²" square-fn]
                        ["sin(x)" sin-fn]
                        ["exp(x)" exp-fn]]
        test-points [1 10 100 10000]
        implementations [["Simple" (fn [f n x] ((simple-integral f n) x))]
                         ["Memoized" (fn [f n x] ((memoized-integral f n) x))]
                         ["Lazy" (fn [f n x] ((lazy-integral f n) x))]]
        iterations 100
        n 1000]
    
    
    (doseq [[f-name f] test-functions]
      (doseq [x test-points]
          (println (str f-name " (x=" x ")"))
          (println "simple-integral")
          (measure-average-time #((simple-integral f n) x) iterations)
          (println "memoized-integral")
          (measure-average-time #((memoized-integral f n) x) iterations)
          (println "lazy-integral")
          (measure-average-time #((lazy-integral f n) x) iterations)
      ))
))

(defn -main
  [& args]
  (benchmark-integral-table)
)