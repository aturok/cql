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
    (let [withand (condition :a = :b and :c = :d)]
    	(is (= true
    			(withand {:a 1 :b 1 :c 1 :d 1})))
    	(is (= false
    			(withand {:a 1 :b 1 :c 2 :d 1})))))
  (testing "handles condition with parenthesis"
    (let [wparenthesis (condition (:c = :d or :b = :c))]
    	(is (= true
    			(wparenthesis {:a 1 :b 1 :c 2 :d 2})))
    	(is (= true
    			(wparenthesis {:a 1 :b 1 :c 1 :d 0})))
    	(is (= false
    			(wparenthesis {:a 1 :b 1 :c 2 :d 3})))
    	(is (= false
    			(wparenthesis {:a 1 :b 0 :c 2 :d 0})))))
  (testing "handles condition with prioritization"
    (let [complex (condition :a = :b and (:c = :d or :b = :c))]
    	(is (= true
    			(complex {:a 1 :b 1 :c 2 :d 2})))
    	(is (= true
    			(complex {:a 1 :b 1 :c 1 :d 0})))
    	(is (= false
    			(complex {:a 1 :b 1 :c 2 :d 3})))
    	(is (= false
    			(complex {:a 1 :b 0 :c 0 :d 0}))))))
