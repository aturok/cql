(ns cql.core-test
  (:require [clojure.test :refer :all]
            [cql.core :refer :all]))

(deftest condition-test
  (testing "handles simple 2arg"
    (let [aeqb (condition :a = :b)]
    	(is (= true
    			(aeqb {:a 1 :b 1})))
    	(is (= false
    			(aeqb {:a 1 :b 2})))))
  (testing "handles simple condition with and"
    (let [aeqb (condition :a = :b and :c = :d)]
    	(is (= true
    			(aeqb {:a 1 :b 1 :c 1 :d 1})))
    	(is (= false
    			(aeqb {:a 1 :b 1 :c 2 :d 1}))))))
