(ns cql.core)

;Just write a dictionary join to be used like this (join table1 table2 on :t1.a = :t2.a and :t1.b > :t2.b)
;;We need to:
;;1. Make keywords of dict-symbols passed in;
;;2. Extend the keys in tables? Or make table-key pair out of our complex keys?
;;3. Create a join function
;;4. Iterate over the two tables outputing joined thing with extended keywords when joiner evaluates to true.

(defmacro keyed [a]
	`(fn [] [(keyword '~a) a]))

(defmacro join [a b _ & on]
	(let [akey (keyword '~a) bkey# (keyword '~b)]
		`{'akey ~a bkey# ~b}))

;Setup a routine, which transforms the on condition list into a boolean function
;callable like this (condition :a.a = :b.a and (:a.c = :b.c or :a.d = b.d))

(defmacro condition 
	([complex]
		`(fn [t#] ((condition ~@complex) t#)))
	([k1 op k2]
	`(fn [t#] (~op (~k1 t#) (~k2 t#))))
	([k1 op1 k2 link & other]
	`(fn [t#] 
		(~link
			((condition ~k1 ~op1 ~k2) t#)
			((condition ~@other) t#)))))
