(ns task1.1
  (:gen-class))

(defn extend-string
  ([chars string] 
   (if (not (empty? chars))
     (if (not= (last string) (first (first chars)))
          (concat (list (str string (first (first chars)))) (extend-string (rest chars) string))
          (extend-string (rest chars) string))
   )))

(defn extend-strings 
  ([strings chars] 
  (if (not (empty? strings))
    (concat (extend-string chars (first strings)) (extend-strings (rest strings) chars))
  )))

(defn create-strings 
  ([chars n] 
  (if (<= n 0) 
      (list)
      (if (= n 1)
         chars
         (extend-strings (create-strings chars (dec n)) chars)
    ))))

(println (create-strings (list "a" "b" "c") 3))
