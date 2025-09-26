(ns task1.4
(:gen-class))

(defn create-strings [chars n]
  (reduce (fn [strings i]
            (reduce concat
                    (map (fn [s] 
                           (map (fn [c] (str s c))
                                (filter (fn [c] (not= (last s) (first c))) chars)))
                         strings)))
          (list "")
          (range n)))
   
(println (create-strings (list "a" "b" "c") 3))
