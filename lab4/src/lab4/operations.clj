(ns lab4.operations)

(defn expr-to-str [expr])

(defn get-type
  "Returns the type of expression"
  [expr]
  (keyword (first expr)))

(defn same-type?
  "Check that expr1 and expr2 are of the same type"
  [expr1 expr2]
  (= (first expr1) (first expr2)))

(defn args
  "Get arguments of operation"
  [expr]
  (rest expr))

(defn first-arg
  "Get first argument of operation"
  [expr]
  (first (rest expr)))

(defn second-arg
  "Get second argument of operation"
  [expr]
  (second (rest expr)))

; constant expression

(defn constant
  "Constructor for constant"
  [value]
  {:pre [(or (= value 0) (= value 1))]}
  (list ::const value))

(defn constant?
  "Check if expr is constant"
  [expr]
  (= (first expr) ::const))

(defn constant-value
  "Get constant value"
  [expr]
  (second expr))

(defn constant-to-str
  "Converts constant to string"
  [expr]
  {:pre [(constant? expr)]}
  (str (constant-value expr))
  )

; variable expression

(defn variable
  "Constructor for variable"
  [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable?
  "Check if expr is variable"
  [expr]
  (= (first expr) ::var))

(defn variable-name
  "Get variable name"
  [var]
  (name (second var)))

(defn same-variables?
  "Check if two variable are the same"
  [var1 var2]
  (and
    (variable? var1)
    (variable? var2)
    (= (variable-name var1)
       (variable-name var2))))

(defn variable-to-str
  "Converts variable to string"
  [expr]
  {:pre [(variable? expr)]}
  (variable-name expr))

; negation expression

(defn logic-not
  "Constructor for negation"
  [expr]
  (list ::not expr))

(defn logic-not?
  "Check if expression is negation"
  [expr]
  (= (first expr) ::not))

(defn logic-not-to-str
  "Converts negation to string"
  [expr]
  {:pre [(logic-not? expr)]}
  (str "!"
       (expr-to-str (first-arg expr))))

; disjunction expression

(defn logic-or
  "Constructor for disjunction"
  [expr & rest]
  (if (empty? rest)
    expr
    (cons ::or (cons expr rest))))

(defn logic-or?
  "Check if expression is disjunction"
  [expr]
  (= (first expr) ::or))

(defn logic-or-to-str
  "Converts disjunction to string"
  [expr]
  {:pre [logic-or? expr]}
  (str "("
       (reduce
         (fn [exprs-str another-expr-str]
           (str exprs-str " || " another-expr-str))
         (expr-to-str (first-arg expr))
         (map
           (fn [expr]
             (expr-to-str expr))
           (rest (args expr))))
       ")"))

; conjunction expression

(defn logic-and
  "Constructor for conjunction"
  [expr & rest]
  (if (empty? rest)
    expr
    (cons ::and (cons expr rest))))

(defn logic-and?
  "Check if expression is conjunction"
  [expr]
  (= (first expr) ::and))

(defn logic-and-to-str
  "Converts conjunction to string"
  [expr]
  {:pre [logic-and? expr]}
  (str "("
       (reduce
         (fn [exprs-str another-expr-str]
           (str exprs-str " && " another-expr-str))
         (expr-to-str (first (args expr)))
         (map
           (fn [expr]
             (expr-to-str expr))
           (rest (args expr))))
       ")"))

; implication expression

(defn logic-impl
  "Constructor for implication"
  [expr1 expr2]
  (list ::impl expr1 expr2))

(defn logic-impl?
  "Check if expression is implication"
  [expr]
  (= (first expr) ::impl))

(defn logic-impl-to-str
  "Converts implication to string"
  [expr]
  {:pre [logic-impl? expr]}
  (str "("
       (expr-to-str (first (args expr)))
       " -> "
       (expr-to-str (second (args expr)))
       ")"))

(defn expr-to-str
  "Converting expression to string"
  [expr]
  (let
    [unknown_type "unknown"
    rules-map (hash-map
        ::const constant-to-str
        ::var variable-to-str
        ::not logic-not-to-str
        ::or logic-or-to-str
        ::and logic-and-to-str
        ::impl logic-impl-to-str
    )
    handler (get rules-map (get-type expr) unknown_type)]
    (if (= handler unknown_type)
      (
        (println expr)
        (throw (Exception. "Unknown type of expression"))
      )
      (handler expr)
    )))

(defn print-expr
  [expr]
  (println (expr-to-str expr)))