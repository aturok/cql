(ns cql.core)

(defmacro condo
	([t]
		`true)
	([t complex]
		`(condo ~t ~@complex))
	([t k1 op k2]
		(if (seq? k1)
			`(~op
				(condo ~t ~k1)
				(condo ~t ~k2))
			(let [v1 (if (keyword? k1) `(~k1 ~t) k1)
				  v2 (if (keyword? k2) `(~k2 ~t) k2)]
			`(~op ~v1 ~v2))))
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

(defmacro selector [& what]
	(if (= (first what) '*)
		identity
		(let [fields (set (take-while keyword? what))]
		  `(fn [r#]
				(into {} (filter #(~fields (first %)) r#))))))

(defmacro select [& what]
	(let [source (take-while #(not= 'where %)
					(next (drop-while #(not= 'from %) what)))
		  table (first source)
		  finalsource (if (nil? (next source))
						`(map (partial extend-keys (keyword '~table)) ~table)
						`(~(fnext source) ~table ~@(nnext source)))
		  conditions (next (drop-while #(not= 'where %) what))]
	`(map (selector ~@what)
		(filter (condition ~@conditions) ~finalsource))))
