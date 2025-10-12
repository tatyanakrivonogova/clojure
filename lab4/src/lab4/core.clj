(ns lab4.core
  (:require [lab4.operations :refer :all]
            [lab4.dnf :refer :all]))

(defn -main []
  ;; (print-expr
  ;;   ; ((x -> y) == 1)
  ;;   (logic-eq (logic-impl (variable :x) (variable :y)) (constant 1)))
  ;; (print-expr
  ;;   ; (x || y || z)
  ;;   (logic-or
  ;;     (variable :x)
  ;;     (variable :y)
  ;;     (variable :z)))
  ;; (print-expr
  ;;   ((convert-implication)
  ;;   ; (x -> y) -> (0 -> z) = !(!x || y) || (!0 || z)
  ;;   (logic-impl
  ;;     (logic-impl (variable :x) (variable :y))
  ;;     (logic-impl (constant 0) (variable :z)))))
  ;; (print-expr
  ;;   (constant-laws
  ;;     ; (x && 0) || (y && 0) = 0 || 0
  ;;     (logic-or
  ;;       (logic-and
  ;;         (variable :x)
  ;;         (constant 0))
  ;;       (logic-and
  ;;         (variable :y)
  ;;         (constant 0)))))
  ;; (print-expr
  ;;   (dnf
  ;;     ; x -> y
  ;;     (logic-impl
  ;;       (variable :x)
  ;;       (variable :y))))
  ;; (print-expr
  ;;   (dnf
  ;;     ; (x && (x || y)) = (x || (x && y))
  ;;     (logic-and
  ;;       (variable :x)
  ;;       (logic-or
  ;;         (variable :x)
  ;;         (variable :y)))))
  ;; (print-expr
  ;;   (dnf
  ;;     (substitute-vals
  ;;       ; (x && y) -> z [x = 1]
  ;;       (logic-impl
  ;;         (logic-and
  ;;           (variable :x)
  ;;           (variable :y))
  ;;         (variable :z))
  ;;       (hash-map :x 1))))

  (print-expr
    (dnf
      (substitute-vals
                 (logic-and (variable :x) (variable :y))
                 {:x 1 :y 0})))
)