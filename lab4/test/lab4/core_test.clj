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
    (is (= (logic-impl (variable :x) (variable :y)) '(:lab4.operations/impl (:lab4.operations/var :x) (:lab4.operations/var :y))))
    (is (= (logic-eq (variable :x) (variable :y)) '(:lab4.operations/eq (:lab4.operations/var :x) (:lab4.operations/var :y))))))

(deftest string-conversion-test
  (testing "Expression to string conversion"
    (is (= (expr-to-str (constant 1)) "1"))
    (is (= (expr-to-str (variable :x)) "x"))
    (is (= (expr-to-str (logic-not (variable :x))) "!x"))
    (is (= (expr-to-str (logic-or (variable :x) (variable :y))) "(x || y)"))
    (is (= (expr-to-str (logic-and (variable :x) (variable :y))) "(x && y)"))
    (is (= (expr-to-str (logic-impl (variable :x) (variable :y))) "(x -> y)"))
    (is (= (expr-to-str (logic-eq (variable :x) (variable :y))) "(x == y)"))))

(deftest main-examples-test
  (testing "Examples from main function"
    
    (testing "Basic implication: x -> y"
      (let [expr (logic-impl (variable :x) (variable :y))
            result (dnf expr)]
        (is (logic-or? result))
        (is (logic-not? (first-arg result)))
        (is (variable? (second-arg result)))))
    
    (testing "Idempotent simplification: x && (x || y)"
      (let [expr (logic-and
                   (variable :x)
                   (logic-or (variable :x) (variable :y)))
            result (dnf expr)]
        (is (logic-or? result))
        (is (variable? (first-arg result)))))
    
    (testing "Substitution: (x && y) -> z with x=1"
      (let [expr (substitute-vals
                   (logic-impl
                     (logic-and (variable :x) (variable :y))
                     (variable :z))
                   {:x 1})
            result (dnf expr)]
        (is (logic-or? result))
        (is (logic-not? (first-arg result)))
        (is (variable? (second-arg result)))))
    
    (testing "Implication chain conversion"
      (let [expr ((convert-implication)
                  (logic-impl
                    (logic-impl (variable :x) (variable :y))
                    (logic-impl (constant 0) (variable :z))))
            result-str (expr-to-str expr)]
        (is (logic-or? expr))
        (is (logic-not? (first-arg expr)))))
    
    (testing "Constant laws: (x && 0) || (y && 0)"
      (let [expr (constant-laws
                   (logic-or
                     (logic-and (variable :x) (constant 0))
                     (logic-and (variable :y) (constant 0))))]
        (is (logic-or? expr))
        (is (= (first-arg expr) (constant 0)))
        (is (= (second-arg expr) (constant 0)))))
    
    (testing "Equality expression"
      (let [expr (logic-eq (logic-impl (variable :x) (variable :y)) (constant 1))
            result-str (expr-to-str expr)]
        (is (logic-eq? expr))))
    
    (testing "Multiple OR expression"
      (let [expr (logic-or (variable :x) (variable :y) (variable :z))
            result-str (expr-to-str expr)]
        (is (logic-or? expr))
        (is (= (count (args expr)) 3))))))

(deftest implication-conversion-test
  (testing "Implication conversion"
    (let [impl-expr (logic-impl (variable :x) (variable :y))
          converted ((convert-implication) impl-expr)]
      (is (logic-or? converted))
      (is (logic-not? (first-arg converted)))
      (is (variable? (second-arg converted))))))

(deftest negation-equivalencies-test
  (testing "Negation equivalencies"
    ;; !(x || y) = !x && !y
    (let [expr (logic-not (logic-or (variable :x) (variable :y)))
          result (negation-equivalencies expr)]
      (is (logic-and? result)))
    
    ;; !(x && y) = !x || !y
    (let [expr (logic-not (logic-and (variable :x) (variable :y)))
          result (negation-equivalencies expr)]
      (is (logic-or? result)))
    
    ;; !!x = x
    (let [expr (logic-not (logic-not (variable :x)))
          result (negation-equivalencies expr)]
      (is (variable? result)))
    
    ;; !0 = 1
    (let [expr (logic-not (constant 0))
          result (negation-equivalencies expr)]
      (is (= result (constant 1))))
    
    ;; !1 = 0
    (let [expr (logic-not (constant 1))
          result (negation-equivalencies expr)]
      (is (= result (constant 0))))))

(deftest distributivity-test
  (testing "Distributivity law"
    ;; (x || y) && z = (x && z) || (y && z)
    (let [expr (logic-and 
                 (logic-or (variable :x) (variable :y))
                 (variable :z))
          result (distributivity-law expr)]
      (is (logic-or? result))
      (is (= (count (args result)) 2)))))

(deftest idempotent-test
  (testing "Idempotent law"
    ;; x && x = x
    (let [expr (logic-and (variable :x) (variable :x))
          result (idempotent-law expr)]
      (is (variable? result)))
    
    ;; x || x = x
    (let [expr (logic-or (variable :x) (variable :x))
          result (idempotent-law expr)]
      (is (variable? result)))))

(deftest constant-laws-test
  (testing "Constant laws"
    ;; x && 1 = x
    (let [expr (logic-and (variable :x) (constant 1))
          result (constant-laws expr)]
      (is (variable? result)))
    
    ;; x && 0 = 0
    (let [expr (logic-and (variable :x) (constant 0))
          result (constant-laws expr)]
      (is (= result (constant 0))))
    
    ;; x || 1 = 1
    (let [expr (logic-or (variable :x) (constant 1))
          result (constant-laws expr)]
      (is (= result (constant 1))))
    
    ;; x || 0 = x
    (let [expr (logic-or (variable :x) (constant 0))
          result (constant-laws expr)]
      (is (variable? result)))))

(deftest dnf-conversion-test
  (testing "DNF conversion"
    ;; x -> y = !x || y
    (let [expr (logic-impl (variable :x) (variable :y))
          result (dnf expr)]
      (is (logic-or? result))
      (is (logic-not? (first-arg result)))
      (is (variable? (second-arg result))))
    
    ;; (x && y) || z should stay the same in DNF
    (let [expr (logic-or 
                 (logic-and (variable :x) (variable :y))
                 (variable :z))
          result (dnf expr)]
      (is (logic-or? result)))
    
    ;; Complex expression
    (let [expr (logic-impl 
                 (logic-and (variable :x) (variable :y))
                 (variable :z))
          result (dnf expr)]
      (is (logic-or? result)))))

(deftest substitution-test
  (testing "Variable substitution"
    ;; x [x=1] = 1
    (let [expr (variable :x)
          result (substitute-vals expr {:x 1})]
      (is (= result (constant 1))))
    
    ;; (x && y) [x=1] = (1 && y)
    (let [expr (logic-and (variable :x) (variable :y))
          result (substitute-vals expr {:x 1})]
      (is (logic-and? result))
      (is (= (first-arg result) (constant 1)))
      (is (variable? (second-arg result))))))

(deftest complex-expressions-test
  (testing "Complex expressions"
    ;; De Morgan's law: !(x && y) = !x || !y
    (let [expr (logic-not (logic-and (variable :x) (variable :y)))
          result (dnf expr)]
      (is (logic-or? result))
      (let [args (args result)]
        (is (every? logic-not? args))))
    
    ;; Multiple operations
    (let [expr (logic-impl 
                 (logic-or (variable :x) (variable :y))
                 (logic-and (variable :z) (variable :w)))
          result (dnf expr)]
      (is (logic-or? result)))
    
    ;; (x -> y) && (y -> z)
    (let [expr (logic-and
                 (logic-impl (variable :x) (variable :y))
                 (logic-impl (variable :y) (variable :z)))
          result (dnf expr)]
      (is (logic-or? result)))
    
    ;; Multiple OR and AND: (x && y) || (z && w)
    (let [expr (logic-or
                 (logic-and (variable :x) (variable :y))
                 (logic-and (variable :z) (variable :w)))
          result (dnf expr)]
      (is (logic-or? result)))
    
    ;; Nested implications: x -> (y -> z)
    (let [expr (logic-impl
                 (variable :x)
                 (logic-impl (variable :y) (variable :z)))
          result (dnf expr)]
      (is (logic-or? result)))
    
    ;; With constants: (x || 0) && (y || 1)
    (let [expr (logic-and
                 (logic-or (variable :x) (constant 0))
                 (logic-or (variable :y) (constant 1)))
          result (dnf expr)]
      (is (logic-or? result)))
    
    ;; Triple implication: (x -> y) -> z
    (let [expr (logic-impl
                 (logic-impl (variable :x) (variable :y))
                 (variable :z))
          result (dnf expr)]
      (is (logic-or? result)))
    
    ;; Complex nested expression: ((x && y) -> z) && (x || y)
    (let [expr (logic-and
                 (logic-impl
                   (logic-and (variable :x) (variable :y))
                   (variable :z))
                 (logic-or (variable :x) (variable :y)))
          result (dnf expr)]
      (is (logic-or? result)))))

(deftest substitution-examples-test
  (testing "Substitution examples"
    ;; Multiple substitutions: (x && y) [x=1, y=0]
    (let [expr (substitute-vals
                 (logic-and (variable :x) (variable :y))
                 {:x 1 :y 0})]
      (is (logic-and? expr))
      (is (= (first-arg expr) (constant 1)))
      (is (= (second-arg expr) (constant 0))))
    
    ;; Substitution with implication: (x -> y) [x=0]
    (let [expr (substitute-vals
                 (logic-impl (variable :x) (variable :y))
                 {:x 0})]
      (is (logic-impl? expr))
      (is (= (first-arg expr) (constant 0)))
      (is (variable? (second-arg expr))))))

(deftest edge-cases-test
  (testing "Edge cases"
    ;; Single variable
    (let [expr (variable :x)
          result (dnf expr)]
      (is (variable? result)))
    
    ;; Constant only
    (let [expr (constant 1)
          result (dnf expr)]
      (is (= result (constant 1))))
    
    ;; Nested same operations
    (let [expr (logic-or 
                 (variable :x)
                 (logic-or (variable :y) (variable :z)))
          result (dnf expr)]
      (is (logic-or? result))
      (is (= (count (args result)) 3)))))
