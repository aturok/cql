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

(defmacro condo
	([t complex]
		`(condo ~t ~@complex))
	([t k1 op k2]
		(if (seq? k1)
			`(~op
				(condo ~t ~k1)
				(condo ~t ~k2))
			`(~op (~k1 ~t) (~k2 ~t))))
	([t k1 op1 k2 link & other]
		(if (seq? k1)
			`(~op1
				(condo ~t ~k1)
				(condo ~t ~k2 ~link ~@other))
			`(~link
				(condo ~t ~k1 ~op1 ~k2)
				(condo ~t ~@other)))))

(defmacro condition 
	[& conditions]
	`(fn [t#]
		(condo t# ~@conditions)))

(defn keystr [k]
	(subs (str k) 1))

(defn combine-key [left right]
	(keyword
		(str (keystr left) "." (keystr right))))

(defn extend-keys [tablename table]
	(into {} (map (fn [[k v]] [(combine-key tablename k) v]) table)))


(defmacro inner-join [table1 table2 _ & conditions]
	`(filter identity
		(for [r1# (map (partial extend-keys (keyword '~table1)) ~table1)
		      r2# (map (partial extend-keys (keyword '~table2)) ~table2)]
		   (let [combined# (into r1# r2#)]
		   		(if (condo combined# ~@conditions)
		   			combined#
		   			nil)))))

(defmacro selector [f & stuff]
	(if (= f '*)
		identity
		(let [otherkeys (take-while keyword? stuff)
			  ks (apply vector (conj otherkeys f))]
			  `(fn [r#]
					(into {} (map (fn [k#] [k# (k# r#)]) ~ks))))))

(defmacro select
	([what from table]
	`(let [result# (map (partial extend-keys (keyword '~table)) ~table)
		   selector# (selector ~what)]
		(map selector# result#))))
