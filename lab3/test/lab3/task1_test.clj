(ns lab3.task1-test
  (:require [clojure.test :refer :all]
            [lab3.common :refer :all]
            [lab3.task1 :refer :all]))

(deftest test-parallel-filter
  (testing "parallel-filter for even? predicate"
    (let [coll (range 10)
          result (parallel-filter even? 2 coll)]
      (is (= [0 2 4 6 8] result))
      (is (every? even? result))))

  (testing "parallel-filter for odd? predicate"
    (let [coll (range 10)
          result (parallel-filter odd? 3 coll)]
      (is (= [1 3 5 7 9] result))
      (is (every? odd? result))))

  (testing "parallel-filter for empty collection"
    (is (empty? (parallel-filter even? 2 []))))

  (testing "parallel-filter for single thread"
    (let [coll [1 2 3 4 5]
          result (parallel-filter even? 1 coll)]
      (is (= [2 4] result))))

  (testing "parallel-filter for heavy-predicate"
    (let [coll [1 2 3 4 5 6]
          result (parallel-filter heavy-predicate 3 coll)]
      (is (= [2 4 6] result)))

  (testing "parallel-filter keeps original order of elements"
    (let [coll [5 2 8 1 9 4 7 3 6]
          result (parallel-filter even? 3 coll)]
      (is (= [2 8 4 6] result))))))

(deftest edge-case-tests
  (testing "One element in collection"
    (is (= [2] (parallel-filter even? 2 [2]))))

  (testing "All elements matches filter"
    (let [coll [2 4 6 8]]
      (is (= coll (parallel-filter even? 2 coll)))))

  (testing "No element matches filter"
    (let [coll [1 3 5 7]]
      (is (empty? (parallel-filter even? 2 coll)))))

  (testing "Big number of threads"
    (let [coll [1 2 3]]
      (is (= [2] (parallel-filter even? 10 coll))))))