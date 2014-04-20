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
                (long {:a 1 :b 2 :c 3 :d 1})))))
  (testing "creates always true condition for no args"
    (let [truth (condition)]
        (is (= true
                (truth {})))
        (is (= true
                (truth {:a 1 :b 1})))
        (is (= true
                (truth {:a 2 :b 2 :c 2 :d 4})))))
  (testing "handles numeric literals"
    (let [less5 (condition :a < 5)]
      (is (= true
            (less5 {:a 4})))
      (is (= false
            (less5 {:a 5})))
      (is (= false
            (less5 {:a 6})))))
  (testing "handles bound numeric literals"
    (let [five 5 less5 (condition :a < five)]
      (is (= true
            (less5 {:a 4})))
      (is (= false
            (less5 {:a 5})))
      (is (= false
            (less5 {:a 6})))))
  (testing "handles calculations"
    (let [less5 (condition :a < (- 11 6))]
      (is (= true
            (less5 {:a 4})))
      (is (= false
            (less5 {:a 5})))
      (is (= false
            (less5 {:a 6}))))))

(deftest extend-keys-test
    (testing "extends dict keys with dict name"
        (is (= {:t.a 1 :t.b 2}
               (extend-keys :t {:a 1 :b 2}))))
    (testing "works ok on empty dict"
        (is (= {}
               (extend-keys :t {})))))

(deftest inner-join-test
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
                   (select * from table)))))
    (testing "selects only specified keys"
        (let [table [{:k1 1 :k2 3 :k3 300}
                     {:k1 2 :k2 2 :k3 600}
                     {:k1 3 :k2 2 :k3 900}]]
            (is (= [{:table.k1 1} {:table.k1 2} {:table.k1 3}]
                   (select :table.k1 from table)))
            (is (= [{:table.k1 1 :table.k3 300}
                    {:table.k1 2 :table.k3 600}
                    {:table.k1 3 :table.k3 900}]
                   (select :table.k1 :table.k3 from table)))
            (is (= (map (partial extend-keys :table) table)
                   (select :table.k1 :table.k3 :table.k2 from table)))))
    (testing "filters star results based on condition"
        (let [table [{:k1 1 :k2 3 :k3 300}
                     {:k1 2 :k2 2 :k3 600}
                     {:k1 3 :k2 2 :k3 900}]]
            (is (= [{:table.k1 1 :table.k2 3 :table.k3 300}
                    {:table.k1 2 :table.k2 2 :table.k3 600}]
                   (select * from table where :table.k1 <= :table.k2)))
            (is (= [{:table.k1 1 :table.k2 3 :table.k3 300}
                    {:table.k1 2 :table.k2 2 :table.k3 600}
                    {:table.k1 3 :table.k2 2 :table.k3 900}]
                   (select * from table where :table.k1 < :table.k3)))
            (is (= [{:table.k1 2 :table.k2 2 :table.k3 600}]
                   (select * from table where :table.k1 = :table.k2)))
            (is (empty?
                   (select * from table where :table.k1 > :table.k3)))))
    (testing "filters keyed results based on condition"
        (let [table [{:k1 1 :k2 3 :k3 300}
                     {:k1 2 :k2 2 :k3 600}
                     {:k1 3 :k2 2 :k3 900}]]
            (is (= [{:table.k1 1}
                    {:table.k1 2}]
                   (select :table.k1 from table where :table.k1 <= :table.k2)))
            (is (= [{:table.k1 2 :table.k2 2}
                    {:table.k1 3 :table.k2 2}]
                   (select :table.k1 :table.k2
                    from table
                    where :table.k1 = :table.k2
                      or  :table.k2 < :table.k1))))))
