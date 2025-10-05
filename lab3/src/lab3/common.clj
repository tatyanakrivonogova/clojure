(ns lab3.common)

(defn heavy-predicate
  [x]
  (Thread/sleep 1)
  (even? x))

(defn create-collection-partitioner
  [num-partitions coll]
  (let [collection-size (count coll)
        base-chunk-size (quot collection-size num-partitions)
        remainder-count (mod collection-size num-partitions)
        partitions (->>
                    (iterate
                      (fn [[current-partition remaining-items partition-index]]
                        (if (< partition-index remainder-count)
                          [(take (inc base-chunk-size) remaining-items)
                           (drop (inc base-chunk-size) remaining-items)
                           (inc partition-index)]
                          [(take base-chunk-size remaining-items)
                           (drop base-chunk-size remaining-items)
                           (inc partition-index)]))
                      [coll coll 0])
                    (map first))]
    (fn [partition-number]
      (nth partitions partition-number))))

(defn partition-collection
  [num-partitions coll]
  (let [get-partition (create-collection-partitioner num-partitions coll)]
    (->>
      (range 1 (inc num-partitions))
      (map get-partition))))