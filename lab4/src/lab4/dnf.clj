(ns lab4.dnf
  (:require [lab4.operations :refer :all]))

(defn convert-operations-by-type
  "Returns function for converting all occurrences of the specified type operation inside an expression with convert-f"
  [instance-of-type? convert-f]
  (fn convert-logic-op [expr]
    (cond
      (instance-of-type? expr) (convert-f convert-logic-op expr)
      (constant? expr) expr
      (variable? expr) expr
      :else (let [expr-type (get-type expr)
                  unknow-constructor "unknow"
                  constructors (hash-map
                    :lab4.operations/not logic-not
                    :lab4.operations/or logic-or
                    :lab4.operations/and logic-and
                    :lab4.operations/impl logic-impl
                    :lab4.operations/nand logic-nand)
                  constructor (get constructors expr-type unknow-constructor)
                  converted-args (map
                                          convert-logic-op
                                          (args expr))]
              (if (= constructor unknow-constructor)
                (throw (Exception. (str "Can not find constructor for " expr-type)))
                (apply constructor converted-args)
              ))
      )))

(defn convert-implication
  "Convert implication (x -> y = !x || y)"
  []
  (convert-operations-by-type
    logic-impl?
    (fn [convert-logic-op expr]
      (logic-or
        (logic-not (convert-logic-op (first-arg expr)))
        (convert-logic-op (second-arg expr))))))

(defn convert-nand
  "Convert NAND (x NAND y = !(x && y))"
  []
  (convert-operations-by-type
    logic-nand?
    (fn [convert-logic-op expr]
      (logic-not
        (logic-and
          (convert-logic-op (first-arg expr))
          (convert-logic-op (second-arg expr)))))))

(defn negation-equivalencies
  "!(x || y) = !x && !y
   !(x && y) = !x || !y
   !(!x) = x
   !0 = 1; !1 = 0"
  [expr]
    (cond
        (logic-not? expr) (let [sub-expr (first-arg expr)]
        (cond
        ; !(expr1 || expr2) = !expr1 && !expr2
        (logic-or? sub-expr) (apply logic-and (map (fn [e] (negation-equivalencies (logic-not e))) (args sub-expr)))
        ; !(expr1 && expr2) = !expr1 || !expr2
        (logic-and? sub-expr) (apply logic-or (map (fn [e] (negation-equivalencies (logic-not e))) (args sub-expr)))
        ; !(!expr1) = sub-expr1
        (logic-not? sub-expr) (negation-equivalencies (first-arg sub-expr))
        ; !0 = 1; !1 = 0
        (constant? sub-expr) (if (= sub-expr (constant 0))
                                (constant 1)
                                (constant 0))
        :else expr))
      (logic-or? expr) (apply logic-or (map negation-equivalencies (args expr)))
      (logic-and? expr) (apply logic-and (map negation-equivalencies (args expr)))
      :else expr))

(defn distributivity-law
  "(x || y) && (z || w) = (x && z) || (y && z) || (x && w) || (y && w)"
  [expr]
  (cond
    (logic-and? expr) 
        (let [disjunction-exprs (filter
                                logic-or?
                                (args expr))
            not-disjunction-exprs (filter
                                    (fn [e] (not (logic-or? e)))
                                    (args expr))]
            (if (empty? disjunction-exprs)
                (apply logic-and (map distributivity-law (args expr)))
                (let [disjunction (first disjunction-exprs)
                      rest-expr (apply logic-and (concat
                                    not-disjunction-exprs
                                    (drop 1 disjunction-exprs)))]
                (apply logic-or (map
                                    (fn [e]
                                        (distributivity-law (logic-and e rest-expr)))
                                    (args disjunction))))
            ))
    (logic-or? expr) (apply logic-or (map distributivity-law (args expr)))
    :else expr
    )
  )

(defn sort-args
  "Returns list of exprs of the same type on top level of expression
  sort-args a && (b && (c || (d && e))) = (list a, b, (c || (d && e)))"
  [expr]
  (if (or (constant? expr) (variable? expr))
    (list expr)
    (mapcat
      (fn [inner-expr]
        (if (same-type? expr inner-expr)
          (sort-args inner-expr)
          (list inner-expr)
          ))
      (args expr)
      )))

(defn decompose
  "x && (y && z) = x && y && z"
  [expr]
  (if (or (logic-or? expr) (logic-and? expr))
    (cons (get-type expr) (map decompose (sort-args expr)))
    expr))

(defn idempotent-law
  "x && x = x
   x || x = x"
  [expr]
  (cond
    (logic-and? expr) (apply logic-and (map idempotent-law (distinct (args (decompose expr)))))
    (logic-or? expr) (apply logic-or (map idempotent-law (distinct (args (decompose expr)))))
    (logic-not? expr) (logic-not (idempotent-law (first-arg expr)))
    :else expr))

(defn constant-laws
  "x && 1 = x
   x && 0 = 0
   x || 1 = 1
   x || 0 = x"
  [expr]
  (let [expr-args-has-constant (fn [expr c]
                            (some
                              (fn [e]
                                (and
                                  (constant? e)
                                  (= e c)))
                              (args expr)))]
    (cond
      (logic-and? expr) (cond
        (expr-args-has-constant expr (constant 0)) (constant 0)
        (expr-args-has-constant expr (constant 1)) (let [expr (idempotent-law expr)]
            (if (logic-and? expr)
                (constant-laws
                    (apply
                        ;; ... && 1 = ...
                        logic-and
                            (filter
                                (fn [e]
                                    (not (and
                                        (constant? e)
                                        (= e (constant 1)))))
                                (args expr))
                    ))
                expr
            ))
        :else (apply logic-and (map constant-laws (args expr)))
      )
      (logic-or? expr) (cond
        (expr-args-has-constant expr (constant 1)) (constant 1)
        (expr-args-has-constant expr (constant 0)) (let [expr (idempotent-law expr)]
            (if (logic-or? expr)
                (constant-laws
                    (apply
                        ;; ... || 0 = ...
                        logic-or
                            (filter
                                (fn [e]
                                    (not (and
                                        (constant? e)
                                        (= e (constant 0)))))
                                (args expr))
                    ))
                expr
                ))
        :else (apply logic-or (map constant-laws (args expr)))
      )
      :else expr
    )
  )
)

(defn replace-operations-with-base
  [expr]
  (let [converting-rules (list (convert-implication) (convert-nand))]
    (reduce
        (fn [expr rule]
            (rule expr))
        expr
        converting-rules)))

(defn dnf
  "Convert logic expression to DNF"
  [expr]
  (->>
        (replace-operations-with-base expr)
        (negation-equivalencies)
        (distributivity-law)
        (idempotent-law)
        (constant-laws)
))

(defn substitute-values
  "Substitute values of variables to expression"
  [expr vars-map]
  (cond
    (variable? expr) (if (contains? vars-map (first-arg expr))
                         (constant (get vars-map (first-arg expr)))
                         expr)
    (constant? expr) expr
    :else (let [expr-type (get-type expr)
                unknown-constructor "unknown"
                constructors (hash-map
                    :lab4.operations/not logic-not
                    :lab4.operations/or logic-or
                    :lab4.operations/and logic-and
                    :lab4.operations/impl logic-impl
                    :lab4.operations/nand logic-nand)
                constructor (get constructors expr-type unknown-constructor)
                substituted-args (map
                                          (fn [e]
                                            (substitute-values e vars-map))
                                          (args expr))
                ]
            (if (= constructor unknown-constructor)
              (throw (Exception. (str "Can not find constructor for " expr-type)))
              (apply constructor substituted-args)
            ))
    ))