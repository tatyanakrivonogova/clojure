(ns task1.3
(:gen-class))

(defn my-map [func coll]
  (reduce (fn [acc item]
            (concat acc [(func item)]))
          []
          coll))
      
(defn my-filter [pred coll]
  (reduce (fn [acc item]
            (if (pred item)
              (concat acc [item])
               acc))
          []
          coll))

(println (map inc [1,2,3,4]))
(println (my-map inc [1,2,3,4]))

(println (filter even? [1,2,3,4]))
(println (my-filter even? [1,2,3,4]))
