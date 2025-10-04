(ns task1.2
  (:gen-class))

(defn extend-string
  ([chars string] (extend-string chars string (list)))
  ([chars string acc] 
   (if (empty? chars)
     acc
     (recur (rest chars)
            string
            (if (not= (last string) (first (first chars)))
              (concat acc (list (str string (first (first chars)))))
              acc)))))

(defn extend-strings 
  ([strings chars] (extend-strings strings chars (list)))
  ([strings chars acc] 
   (if (empty? strings)
     acc
     (recur (rest strings)
            chars
            (concat acc (extend-string chars (first strings)))))))

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

(println (create-strings (list "a" "b" "c") 3))
