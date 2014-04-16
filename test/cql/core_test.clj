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
  (testing "handles simple 2arg with parenthesis"
    (let [aeqb (condition (:a = :b))]
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
  (testing "handles obsolete paranthesis correctly"
    (let [withand (condition (:a = :b) and (:c = :d))]
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
    			(complex {:a 1 :b 0 :c 0 :d 0})))))
  (testing "handles condition with prioritization (inversed)"
    (let [complex (condition (:c = :d or :b = :c) and :a = :b)]
        (is (= true
                (complex {:a 1 :b 1 :c 2 :d 2})))
        (is (= true
                (complex {:a 1 :b 1 :c 1 :d 0})))
        (is (= false
                (complex {:a 1 :b 1 :c 2 :d 3})))
        (is (= false
                (complex {:a 1 :b 0 :c 0 :d 0})))))
  (testing "handles condition with much prioritization"
    (let [complex (condition (:c = :d or :b = :c) and (:a = :b or :a = :c))]
        (is (= true
                (complex {:a 1 :b 1 :c 2 :d 2})))
        (is (= true
                (complex {:a 1 :b 1 :c 1 :d 0})))
        (is (= false
                (complex {:a 1 :b 1 :c 2 :d 3})))
        (is (= false
                (complex {:a 1 :b 0 :c 0 :d 0})))))
  (testing "handles long and/or correctly"
    (let [long (condition :c = :d or :b = :c and :a = :b or :a = :c)]
        (is (= true
                (long {:a 1 :b 1 :c 2 :d 2})))
        (is (= true
                (long {:a 1 :b 1 :c 1 :d 0})))
        (is (= true
                (long {:a 2 :b 2 :c 2 :d 4})))
        (is (= false
                (long {:a 1 :b 2 :c 3 :d 4})))
        (is (= false
                (long {:a 1 :b 2 :c 3 :d 1}))))))

(deftest extend-keys-test
    (testing "extends dict keys with dict name"
        (is (= {:t.a 1 :t.b 2}
               (extend-keys :t {:a 1 :b 2})))))

(deftest join-test
    (testing "simple condition with simple dicts"
        (let [a [{:k1 1} {:k1 2} {:k1 3}]
              b [{:k2 4} {:k2 1} {:k2 3}]]
            (is (= [{:a.k1 1 :b.k2 1}
                    {:a.k1 3 :b.k2 3}]
                    (inner-join a b on :a.k1 = :b.k2)))))
    (testing "treats similar keys properly"
        (let [a [{:k1 1 :k2 101} {:k1 2 :k2 102} {:k1 3 :k2 103}]
              b [{:k1 204 :k2 4} {:k1 201 :k2 1} {:k1 203 :k2 3}]]
            (is (= [{:a.k1 1 :a.k2 101 :b.k2 1 :b.k1 201}
                    {:a.k1 3 :a.k2 103 :b.k2 3 :b.k1 203}]
                    (inner-join a b on :a.k1 = :b.k2))))))

(deftest select-test
    (testing "selects everything with star and without wheres/joins"
        (let [table [{:k1 1 :k2 3} {:k1 2 :k2 2} {:k1 3 :k2 2}]]
            (is (= (map (partial extend-keys :table) table)
                   (select * from table))))))
