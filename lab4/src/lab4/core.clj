(ns lab4.core
  (:require [lab4.operations :refer :all]
            [lab4.dnf :refer :all]))

(defn -main []

  (print-expr
    (dnf
    (substitute-values (logic-and
                 (logic-impl
                   (logic-and (variable :x) (variable :y))
                   (variable :z))
                 (logic-or (variable :x) (variable :y))) {:x 1 :y 0})
      ))
)