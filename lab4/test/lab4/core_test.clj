(ns lab4.core-test
  (:require [clojure.test :refer :all]
            [lab4.core :refer :all]
            [lab4.operations :refer :all]
            [lab4.dnf :refer :all]))

(deftest basic-construction-test
  (testing "Basic expression construction"
    (is (= (constant 1) '(:lab4.operations/const 1)))
    (is (= (variable :x) '(:lab4.operations/var :x)))
    (is (= (logic-not (variable :x)) '(:lab4.operations/not (:lab4.operations/var :x))))
    (is (= (logic-or (variable :x) (variable :y)) '(:lab4.operations/or (:lab4.operations/var :x) (:lab4.operations/var :y))))
    (is (= (logic-and (variable :x) (variable :y)) '(:lab4.operations/and (:lab4.operations/var :x) (:lab4.operations/var :y))))
    (is (= (logic-impl (variable :x) (variable :y)) '(:lab4.operations/impl (:lab4.operations/var :x) (:lab4.operations/var :y))))))

(deftest string-conversion-test
  (testing "Expression to string conversion"
    (is (= (expr-to-str (constant 1)) "1"))
    (is (= (expr-to-str (variable :x)) "x"))
    (is (= (expr-to-str (logic-not (variable :x))) "!x"))
    (is (= (expr-to-str (logic-or (variable :x) (variable :y))) "(x || y)"))
    (is (= (expr-to-str (logic-and (variable :x) (variable :y))) "(x && y)"))
    (is (= (expr-to-str (logic-impl (variable :x) (variable :y))) "(x -> y)"))))

(deftest implication-conversion-test
  (testing "Implication conversion"
    ;; x -> y = !x || y
    (let [expr (logic-impl (variable :x) (variable :y))
          result ((convert-implication) expr)
          expected-result (logic-or (logic-not (variable :x)) (variable :y))]
        (is (= result expected-result)))
))

(deftest negation-equivalencies-test
  (testing "Negation equivalencies"
    ;; !(x || y) = !x && !y
    (let [expr (logic-not (logic-or (variable :x) (variable :y)))
          result (negation-equivalencies expr)
          expected-result (logic-and (logic-not (variable :x)) (logic-not (variable :y)))]
        (is (= result expected-result)))
    
    ;; !(x && y) = !x || !y
    (let [expr (logic-not (logic-and (variable :x) (variable :y)))
          result (negation-equivalencies expr)
          expected-result (logic-or (logic-not (variable :x)) (logic-not (variable :y)))]
        (is (= result expected-result)))
    
    ;; !!x = x
    (let [expr (logic-not (logic-not (variable :x)))
          result (negation-equivalencies expr)
          expected-result (variable :x)]
        (is (= result expected-result)))
    
    ;; !0 = 1
    (let [expr (logic-not (constant 0))
          result (negation-equivalencies expr)
          expected-result (constant 1)]
        (is (= result expected-result)))
    
    ;; !1 = 0
    (let [expr (logic-not (constant 1))
          result (negation-equivalencies expr)
          expected-result (constant 0)]
        (is (= result expected-result)))
))

(deftest distributivity-test
  (testing "Distributivity law"
    ;; (x || y) && z = (x && z) || (y && z)
    (let [expr (logic-and 
                 (logic-or (variable :x) (variable :y))
                 (variable :z))
          result (distributivity-law expr)
          expected-result (logic-or (logic-and (variable :x) (variable :z)) (logic-and (variable :y) (variable :z)))]
        (is (= result expected-result)))
))

(deftest idempotent-test
  (testing "Idempotent law"
    ;; x && x = x
    (let [expr (logic-and (variable :x) (variable :x))
          result (idempotent-law expr)
          expected-result (variable :x)]
        (is (= result expected-result)))
    
    ;; x || x = x
    (let [expr (logic-or (variable :x) (variable :x))
          result (idempotent-law expr)
          expected-result (variable :x)]
        (is (= result expected-result)))
))

(deftest constant-laws-test
  (testing "Constant laws"
    ;; x && 1 = x
    (let [expr (logic-and (variable :x) (constant 1))
          result (constant-laws expr)
          expected-result (variable :x)]
        (is (= result expected-result)))
    
    ;; x && 0 = 0
    (let [expr (logic-and (variable :x) (constant 0))
          result (constant-laws expr)
          expected-result (constant 0)]
        (is (= result expected-result)))
    
    ;; x || 1 = 1
    (let [expr (logic-or (variable :x) (constant 1))
          result (constant-laws expr)
          expected-result (constant 1)]
        (is (= result expected-result)))
    
    ;; x || 0 = x
    (let [expr (logic-or (variable :x) (constant 0))
          result (constant-laws expr)
          expected-result (variable :x)]
        (is (= result expected-result)))
))

(deftest dnf-conversion-test
  (testing "DNF conversion"
    ;; x -> y = !x || y
    (let [expr (logic-impl (variable :x) (variable :y))
          result (dnf expr)
          expected-result (logic-or (logic-not (variable :x)) (variable :y))]
        (is (= result expected-result)))
    
    ;; (x && y) || z = (x && y) || z 
    (let [expr (logic-or 
                 (logic-and (variable :x) (variable :y))
                 (variable :z))
          result (dnf expr)
          expected-result (logic-or (logic-and (variable :x) (variable :y)) (variable :z))]
        (is (= result expected-result)))
    
    ;; (x && y) -> z = !x || !y || z
    (let [expr (logic-impl 
                 (logic-and (variable :x) (variable :y))
                 (variable :z))
          result (dnf expr)
          expected-result (logic-or (logic-not (variable :x)) (logic-not (variable :y)) (variable :z))]
        (is (= result expected-result)))
))

(deftest substitution-test
  (testing "Variable substitution"
    ;; x [x=1] = 1
    (let [expr (variable :x)
          result (substitute-values expr {:x 1})
          expected-result (constant 1)]
        (is (= result expected-result)))
    
    ;; (x && y) [x=1] = (1 && y)
    (let [expr (logic-and (variable :x) (variable :y))
          result (substitute-values expr {:x 1})
          expected-result (logic-and (constant 1) (variable :y))]
        (is (= result expected-result)))
))

(deftest complex-expressions-test
  (testing "Complex expressions"
    ;; !(x && y) = !x || !y
    (let [expr (logic-not (logic-and (variable :x) (variable :y)))
          result (dnf expr)
          expected-result (logic-or (logic-not (variable :x)) (logic-not (variable :y)))]
        (is (= result expected-result)))
    
    ;; (x || y) -> (z && w) = (!x && !y) || (z && w)
    (let [expr (logic-impl 
                 (logic-or (variable :x) (variable :y))
                 (logic-and (variable :z) (variable :w)))
          result (dnf expr)
          expected-result (logic-or (logic-and (logic-not (variable :x)) (logic-not (variable :y))) (logic-and (variable :z) (variable :w)))]
        (is (= result expected-result)))
    
    ;; (x -> y) && (y -> z) = (!x || y) && (!y || z) = (!y && !x) || (z && !x) || (!y && y) || (z && y)
    (let [expr (logic-and
                 (logic-impl (variable :x) (variable :y))
                 (logic-impl (variable :y) (variable :z)))
          result (dnf expr)
          expected-result (logic-or (logic-and (logic-not (variable :y)) (logic-not (variable :x))) (logic-and (variable :z) (logic-not (variable :x))) (logic-and (logic-not (variable :y)) (variable :y)) (logic-and (variable :z) (variable :y)))]
        (is (= result expected-result)))
    
    ;; (x && y) || (z && w) = (x && y) || (z && w)
    (let [expr (logic-or
                 (logic-and (variable :x) (variable :y))
                 (logic-and (variable :z) (variable :w)))
          result (dnf expr)
          expected-result (logic-or (logic-and (variable :x) (variable :y)) (logic-and (variable :z) (variable :w)))]
        (is (= result expected-result)))
    
    ;; x -> (y -> z) = !x || (!y || z) = !x || !y || z
    (let [expr (logic-impl
                 (variable :x)
                 (logic-impl (variable :y) (variable :z)))
          result (dnf expr)
          expected-result (logic-or (logic-not (variable :x)) (logic-not (variable :y)) (variable :z))]
        (is (= result expected-result)))
    
    ;; (x || 0) && (y || 1) = (y && x) || (1 && x) || (y && 0) || (1 && 0) = (y && x) || x || 0 || 0
    (let [expr (logic-and
                 (logic-or (variable :x) (constant 0))
                 (logic-or (variable :y) (constant 1)))
          result (dnf expr)
          expected-result (logic-or (logic-and (variable :y) (variable :x)) (variable :x) (constant 0) (constant 0))]
        (is (= result expected-result)))
    
    ;; (x -> y) -> z = !(!x || y) || z = (x && !y) || z
    (let [expr (logic-impl
                 (logic-impl (variable :x) (variable :y))
                 (variable :z))
          result (dnf expr)
          expected-result (logic-or (logic-and (variable :x) (logic-not (variable :y))) (variable :z))]
        (is (= result expected-result)))
    
    ;; ((x && y) -> z) && (x || y) = (!x || !y || z) && (x || y) = (x && !x) || (y && !x) || (x && !y) || (y && !y) || (x && z) || (y && z)
    (let [expr (logic-and
                 (logic-impl
                   (logic-and (variable :x) (variable :y))
                   (variable :z))
                 (logic-or (variable :x) (variable :y)))
          result (dnf expr)
          expected-result (logic-or (logic-and (variable :x) (logic-not (variable :x))) (logic-and (variable :y) (logic-not (variable :x))) (logic-and (variable :x) (logic-not (variable :y))) (logic-and (variable :y) (logic-not (variable :y))) (logic-and (variable :x) (variable :z)) (logic-and (variable :y) (variable :z)))]
        (is (= result expected-result)))
))

(deftest substitution-examples-test
  (testing "Substitution examples"
    ;; (x && y) [x=1, y=0] = (1 && 0)
    (let [expr (logic-and (variable :x) (variable :y))
          result (substitute-values
                 expr
                 {:x 1 :y 0})
          expected-result (logic-and (constant 1) (constant 0))]
        (is (= result expected-result)))
    
    ;; (x -> y) [x=0] = (0 -> y)
    (let [expr (logic-impl (variable :x) (variable :y))
          result (substitute-values
                 expr
                 {:x 0})
          expected-result (logic-impl (constant 0) (variable :y))]
        (is (= result expected-result)))

    ;; ((x && y) -> z) && (x || y) [x=1, y=0] = ((1 && 0) -> z) && (1 || 0) = (0 -> z) && 1 = (!0 || z) = 1
    (let [expr (logic-and
                 (logic-impl
                   (logic-and (variable :x) (variable :y))
                   (variable :z))
                 (logic-or (variable :x) (variable :y)))
          result (dnf (substitute-values expr {:x 1 :y 0}))
          expected-result (constant 1)]
        (is (= result expected-result)))
))
