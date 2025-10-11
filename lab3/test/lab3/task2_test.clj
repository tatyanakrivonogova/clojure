(ns lab3.task2-test
  (:require [clojure.test :refer :all]
            [lab3.common :refer :all]
            [lab3.task2 :refer :all]))

(deftest test-lazy-parallel-filter
  (testing "lazy-parallel-filter"
    (let [coll (range 20)
          result (lazy-parallel-filter even? 2 5 coll)]
      (is (= (filter even? coll) result))
      (is (every? even? result))))

  (testing "lazy-parallel-filter with little chunk-size"
    (let [coll (range 10)
          result (lazy-parallel-filter odd? 3 2 coll)]
      (is (= (filter odd? coll) result))))

  (testing "lazy-parallel-filter with big chunk-size"
    (let [coll (range 15)
          result (lazy-parallel-filter #(> % 5) 2 10 coll)]
      (is (= (filter #(> % 5) coll) result))))

  (testing "lazy-parallel-filter with empty collection"
    (is (empty? (lazy-parallel-filter even? 2 5 []))))

  (testing "lazy-parallel-filter keeps original order of elements"
    (let [coll [10 3 7 2 8 1 9 4 6 5]
          result (lazy-parallel-filter even? 2 3 coll)]
      (is (= [10 2 8 4 6] result))))

  (testing "lazy-parallel-filter with heavy-predicate"
    (let [coll (range 12)
          result (lazy-parallel-filter heavy-predicate 3 4 coll)]
      (is (= (filter even? coll) result)))))

(deftest edge-case-tests
  (testing "One element in collection"
    (is (= [2] (lazy-parallel-filter even? 2 1 [2]))))

  (testing "All elements matches filter"
    (let [coll [2 4 6 8]]
      (is (= coll (lazy-parallel-filter even? 2 2 coll)))))

  (testing "No element matches filter"
    (let [coll [1 3 5 7]]
      (is (empty? (lazy-parallel-filter even? 2 2 coll)))))

  (testing "Big number of threads"
    (let [coll [1 2 3]]
      (is (= [2] (lazy-parallel-filter even? 10 1 coll))))))