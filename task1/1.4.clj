(ns task1.4
  (:gen-class))

(defn extend-string [chars string]
  (map #(str string %)
       (filter #(not= (last string) (first %)) chars)))

(defn extend-strings [strings chars]
  (reduce concat
        (map #(extend-string chars %) strings)))

(defn create-strings 
  ([chars n] (create-strings chars n chars))
  ([chars n acc] 
   (if (<= n 0) 
       (list)
       (if (= n 1)
         acc
         (recur chars
                (dec n)
                (extend-strings acc chars))))))

(println (create-strings ["a" "b" "c"] 3))
(println (create-strings (list "a" "b" "c") 5))
(println (create-strings (list "a" "b") 10))
