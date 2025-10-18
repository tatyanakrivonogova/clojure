(ns lab4.core
  (:require [lab4.operations :refer :all]
            [lab4.dnf :refer :all]))

(defn -main []

  (print-expr
    (dnf
    (logic-impl
                 (logic-impl (variable :x) (variable :y))
                 (variable :z))
      ))
)