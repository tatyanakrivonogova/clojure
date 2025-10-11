(ns lab3.common-test
  (:require [clojure.test :refer :all]
            [lab3.common :refer :all]))

(deftest test-heavy-predicate
  (testing "heavy-predicate returns valid result"
    (is (true? (heavy-predicate 2)))
    (is (false? (heavy-predicate 3)))
    (is (true? (heavy-predicate 0)))
    (is (false? (heavy-predicate 1)))))

(deftest test-create-collection-partitioner
  (testing "create-collection-partitioner with equal parts"
    (let [coll [1 2 3 4 5 6]
          get-partition (create-collection-partitioner 3 coll)]
      (is (= [1 2 3 4 5 6] (get-partition 0)))
      (is (= [1 2] (get-partition 1)))
      (is (= [3 4] (get-partition 2)))
      (is (= [5 6] (get-partition 3)))))

  (testing "create-collection-partitioner with unequal parts"
    (let [coll [1 2 3 4 5]
          get-partition (create-collection-partitioner 3 coll)]
      (is (= [1 2 3 4 5] (get-partition 0)))
      (is (= [1 2] (get-partition 1)))
      (is (= [3 4] (get-partition 2)))
      (is (= [5] (get-partition 3)))))

  (testing "create-collection-partitioner for empty collection"
    (let [get-partition (create-collection-partitioner 3 [])]
      (is (= [] (get-partition 0)))
      (is (= [] (get-partition 1)))
      (is (= [] (get-partition 2)))
      (is (= [] (get-partition 3)))))

  (testing "create-collection-partitioner for single element collection"
    (let [get-partition (create-collection-partitioner 3 [42])]
      (is (= [42] (get-partition 0)))
      (is (= [42] (get-partition 1)))
      (is (= [] (get-partition 2)))
      (is (= [] (get-partition 3))))))

(deftest test-partition-collection
  (testing "partition-collection with equal parts"
    (is (= [[1 2] [3 4] [5 6]] 
           (partition-collection 3 [1 2 3 4 5 6]))))

  (testing "partition-collection with unequal parts"
    (is (= [[1 2] [3 4] [5]] 
           (partition-collection 3 [1 2 3 4 5]))))

  (testing "partition-collection for empty collection"
    (is (= [[] []] 
           (partition-collection 2 []))))

  (testing "partition-collection for one thread"
    (is (= [[1 2 3 4 5]] 
           (partition-collection 1 [1 2 3 4 5]))))

  (testing "partition-collection for more threads than elements"
    (is (= [[1] [2] [] []] 
           (partition-collection 4 [1 2])))))