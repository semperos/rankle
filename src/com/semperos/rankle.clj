(ns com.semperos.rankle
  (:refer-clojure :exclude [+ - * / count])
  (:require [clojure.core.memoize :as memo]
            [clojure.pprint :refer [cl-format get-pretty-writer]]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [com.semperos.rankle.html :as html]
            #_[com.semperos.rankle.query :as q]
            [com.semperos.rankle.util :refer [cond-table]]
            #_[net.cgrand.seqexp :as se])
  (:import [java.io Writer]))

(defn ^{:rank ##Inf}
  count
  [coll]
  (clojure.core/count coll))

(defn coll-shape
  [coll shape]
  (if-let [x (first coll)]
    (if (coll? x)
      (recur x (conj shape (count x)))
      shape)))

(defn shape
  [x]
  (cond
    (coll? x)
    (coll-shape x [(count x)])

    (string? x)
    (coll-shape (seq x) [(count x)])

    :else []))

(defn reshape
  [shape coll]
  {:pre [(every? number? shape)]}
  (let [c (cycle coll)
        dims (reverse shape)]
    (loop [dims dims ret nil]
      (if-let [dim (first dims)]
        (recur (next dims)
               (into []
                     (comp (partition-all dim)
                           (map vec))
                     (or ret coll)))
        (first ret)))))

#_(or ret coll)
#_(partition-all rows (partition-all columns coll))

(defn rank
  ([x]
   (if (var? x)
     ((comp :rank meta) x)
     (count (shape x))))
  ([f r]
   (let [f-rank (rank f)]
     (if (= f-rank ##Inf)
       (case r
         0 (fn [arg]
             (if (zero? (rank arg))
               (f arg)
               (map (fn [coll] (map (fn [x] (f [x])) coll)) arg)))
         1 (fn [arg]
             (case (rank arg)
               1 (f arg)
               2 (map f arg)
               3 (map (partial map f) arg)))
         2 (fn [arg]
             (if (= (rank arg) 2)
               (f arg)
               (map f arg)))
         3 f)
       (case r
         0 f
         1 (fn [arg]
             (if (= (rank arg) 1)
               (f arg)
               (map f arg)))
         2 (fn [arg]
             (if (= (rank arg) 2)
               (f arg)
               (map (partial map f) arg)))
         3 (fn [arg]
             (if (= (rank arg) 3)
               (f arg)
               (map (partial map (partial map f)) arg)))))
     )))

(declare ColumnMap->Table column-map)

(defprotocol IAmColumnar
  (columns [this] "Return my columns.")
  (conj-row [this row] "Add a 'row' of data to me.")
  (flip [this] "Transpose me.")
  (row [this idx] "Return a row of my values at `idx`.")
  (row-maps [this] "Return my rows as maps.")
  (slice [this selector] "Access me.")
  (to-table [this] "Return me as a table.")
  (to-column-map [this] "Return me as a column map."))

(defn slice-column-map-by-row
  [cm row-idx]
  (let [vs (vals cm)]
    (if (vector? row-idx)
      (column-map (keys cm)
                  (apply map
                         (fn [& rows] (vec rows))
                         (map (fn [idx] (map #(nth % idx) vs)) row-idx)))
      (column-map (keys cm)
                  (map (fn [r] (vector (nth r row-idx))) vs)))))

(defn slice-column-map-by-column
  [cm col-name]
  (if (vector? col-name)
    (map #(get cm %) col-name)
    (get cm col-name)))

(defn slice-column-map-by-both
  [cm col-name row-idx]
  (let [cvec? (vector? col-name)
        rvec? (vector? row-idx)
        slice-both-vecs (fn []
                          (let [cols-values (map #(get cm %) col-name)]
                            (map (fn [idx] (map #(nth % idx) cols-values)) row-idx)))
        slice-row-vec (fn []
                        (let [col-values (get cm col-name)]
                          (map #(nth col-values %) row-idx)))
        slice-col-vec (fn []
                        (let [cols-values (map #(get cm %) col-name)]
                          (map #(nth % row-idx) cols-values)))
        slice-simple (fn []
                       (nth (get cm col-name) row-idx))]
    (cond-table
     :|             rvec?             (not rvec?)
     :|       cvec? (slice-both-vecs) (slice-col-vec)
     :| (not cvec?) (slice-row-vec)   (slice-simple))))

(defn slice-column-map
  "Slice a `ColumnMap` by the given `selector` (column-row order).

  Selector variants:
  [col-name row-idx]
  [col-name nil]
  [nil      row-idx]"
  [cm [col-name row-idx :as selector]]
  {:pre [(= 2 (count selector))]}
  (let [slice-by-both (fn [] (slice-column-map-by-both cm col-name row-idx))
        slice-by-col  (fn [] (slice-column-map-by-column cm col-name))
        slice-by-row  (fn [] (slice-column-map-by-row cm row-idx))
        error         #(throw (IllegalArgumentException.
                               (str "You must supply a two-item vector as "
                                    "selector in the form [column-name row-index] "
                                    "where either may be nil, but not both.")))]
    (cond-table
     :|                row-idx          (not row-idx)
     :|       col-name (slice-by-both)  (slice-by-col)
     :| (not col-name) (slice-by-row)   (error))))

(defrecord ColumnMap []
  IAmColumnar
  (columns [this] (keys this))
  (conj-row [this row]
    (reduce-kv
     (fn [cm column _]
       (update cm column conj (get row column)))
     this
     this))
  (flip [this]
    (ColumnMap->Table this))
  (slice [this selector]
    (slice-column-map this selector))
  (to-column-map [this] this)
  (to-table [this]
    (ColumnMap->Table this))

  clojure.lang.IFn
  (invoke [this] this)
  (invoke [this selector]
    (slice this selector)))

(defn map->ColumnMap [m]
  (reduce-kv
   (fn [cm k v]
     (assoc cm k (vec v)))
   (ColumnMap.) m))

(defn column-map
  "Given a sequence of column names and a sequence of column value
  sequences, return an equivalent `ColumnMap`."
  ([m] (map->ColumnMap m))
  ([header column-values]
   (when-not (= (count header)
                (count column-values))
     (throw (IllegalArgumentException. "You must supply column values for each column/header.")))
   (->> (interleave header (map vec column-values))
        (apply hash-map)
        (column-map))))

(defn column-map-from-rows
  [header rows]
  (column-map
   (reduce
    (fn [acc row]
      (when-not (= (count row) (count header))
            (throw (ex-info "Rows must be square." {:problem-row row})))
      (if (sequential? row)
        (reduce
         (fn [acc [header value]]
           (update acc header conj value))
         acc
         (partition 2 (interleave header row)))
        (reduce-kv
         (fn [acc column-name value]
           (update acc column-name conj value))
         row)))
    (apply hash-map (interleave header (repeat [])))
    rows)))

(defn value-table
  [t]
  (let [header (first t)
        rows (next t)]
    (flip (column-map-from-rows header rows))))

(defn keyed-on
  "Hard-coded for function tables."
  [column-name t]
  (let [lks (t [nil column-name])
        rks (filterv keyword? (.header t))
        ks (map #(vector %1 %2) lks (cycle rks))]
    (reduce
     (fn [acc [l r :as k]]
       (assoc acc k (t [(.indexOf lks l) r])))
     {}
     ks)))

(defn slice-table-by-row
  [table row-idx]
  {:post [(instance? ColumnMap %)]}
  (let [rs (.rows table)
        header (.header table)]
    (if (vector? row-idx)
      (column-map header (apply map
                                (fn [& rows] (vec rows))
                                (map (fn [idx] (nth rs idx)) row-idx)))
      (column-map header (map vector (nth rs row-idx))))))

(defn slice-table-by-column
  [table col-name]
  (let [cm (.column-map table)]
    (if (vector? col-name)
      (map #(cm [% nil]) col-name)
      ((.column-map table) [col-name nil]))))

(defn slice-table-by-both
  [table row-idx col-name]
  (let [cm (.column-map table)
        header (.header table)
        rs (.rows table)
        cvec? (vector? col-name)
        rvec? (vector? row-idx)
        slice-both-vecs (fn []
                          (let [cols-values (map #(get cm %) col-name)]
                            (column-map col-name
                                        (apply map
                                               (fn [& rows] (vec rows))
                                               (map (fn [idx] (map #(nth % idx) cols-values)) row-idx)))))
        slice-row-vec (fn []
                        (let [values (get cm col-name)
                              vs (map #(nth values %) row-idx)]
                          (column-map [col-name] [vs])))
        slice-col-vec (fn []
                        (let [cols-values (map #(get cm %) col-name)]
                          (map #(nth % row-idx) cols-values)))
        slice-simple (fn []
                       (nth (get cm col-name) row-idx))]
    (cond-table
     :|             cvec?             (not cvec?)
     :|       rvec? (slice-both-vecs) (slice-row-vec)
     :| (not rvec?) (slice-col-vec)   (slice-simple))))

(defn slice-table
  [table [row-idx col-name :as selector]]
  "Slice a `Table` by the given `selector` (row-column order).

  Selector variants:
  [row-idx col-name]
  [row-idx nil]
  [nil     col-name]"
  {:pre [(= 2 (count selector))]}
  (let [slice-by-both (fn [] (slice-table-by-both table row-idx col-name))
        slice-by-row  (fn [] (slice-table-by-row table row-idx))
        slice-by-col  (fn [] (slice-table-by-column table col-name))
        error         #(throw (IllegalArgumentException.
                               (str "You must supply a two-item vector as "
                                    "selector in the form [row-index column-name] "
                                    "where either may be nil, but not both.")))]
    (cond-table
     :|                col-name         (not col-name)
     :|       row-idx  (slice-by-both)  (slice-by-row)
     :| (not row-idx)  (slice-by-col)   (error))))


;; TODO Keyed table as first-class concept
;; TODO Auto-fill ragged column maps/tables
;; TODO Queries
;; TODO Consider core fns on tables (Seq API) vs. column-maps and then the place of separate query language/layer that deals solely in tables.
(deftype Table [header rows column-map]
  IAmColumnar
  (columns [this] (keys column-map))
  (flip [this] column-map)
  (row-maps [this]
    (mapv (fn [row]
            (reduce
             (fn [acc [k v]]
               (assoc acc k v))
             {}
             (map #(vector %1 %2) header row)))
          rows))
  (slice [this selector]
    (slice-table this selector))
  (to-column-map [this] column-map)
  (to-table [this] this)

  clojure.lang.IFn
  (invoke [this] this)
  (invoke [this selector]
    (slice this selector))

  clojure.lang.IPersistentCollection
  (count [this]
    (count rows))
  (cons [this row]
    (ColumnMap->Table (conj-row column-map row)))
  (empty [this]
    (Table. [] [] (column-map {})))

  clojure.lang.Seqable
  (seq [this] rows)

  (equiv [this o]
    (= column-map (to-column-map o)))

  Object
  (equals [this o]
    (clojure.lang.Util/equiv column-map o))
  (hashCode [this]
    (.hashCode column-map)))

(defn ColumnMap->Table
  "Given a `ColumnMap`, return a `Table`."
  [cm]
  (let [header (keys cm)
        rows (reduce-kv
              (fn [rs column values]
                (map (fn [row value] (conj row value)) rs values))
              (repeat (count (val (first cm))) [])
              cm)]
    (Table. header
            rows
            (map->ColumnMap cm))))

(def ->Table ColumnMap->Table)

(def ^{:doc "Create a table from a column map."
       :arglists '([column-map])}
  table ->Table)

(defn- all-keys
  [ms]
  (let [key-set (comp (map keys)
                      (map set))]
    (transduce key-set set/union #{} ms)))

(extend-type clojure.lang.PersistentVector
  IAmColumnar
  (to-column-map [this]
    (when-not (every? map? this)
      (throw (ex-info "To create a table from a vector, each item must be a map."
                      {:this this})))
    (let [columns (all-keys this)
          cm (column-map (apply hash-map (interleave columns (repeat []))))]
      (reduce #(conj-row %1 %2) cm this)))
  (to-table [this]
    (to-table (to-column-map this))))

(defn enmap
  "Return rows of table as maps"
  [table]
  (let [cm (.column-map table)
        columns (keys cm)
        values (vals cm)]
    (apply map
           (fn [& vs]
             (apply hash-map (interleave columns vs)))
           values)))

;;;;;;;;;;;;;;;;;;;
;; Table Queries ;;
;;;;;;;;;;;;;;;;;;;

(defn asc
  "Sort a table in ascending order by column(s). Can be called in a
  standalone fashion or included in a query."
  [table cols]
  (let [cols (if (coll? cols)
               cols
               [cols])]
    (to-table (into [] (sort-by (apply juxt cols) (enmap table))))))

(defn desc
  "Sort a table in descending order by column(s). Can be called in a
  standalone fashion or included in a query."
  [table cols]
  (let [cols (if (coll? cols)
               cols
               [cols])]
    (to-table (into [] (reverse (sort-by (apply juxt cols) (enmap table)))))))

;; TODO Consider if seqexp could be used to tidy this up.
(defn interpret-q-forms
  "Simple query interpreter for Table values."
  [tbl forms]
  ;; select name, line from cct
  (when-not (and (symbol? (first forms))
                 (str/starts-with? (name (first forms)) "select"))
    (throw (ex-info "Your table query is malformed. Review the test suite."
                    {:query forms})))
  (let [forms (vec forms)
        idx-where (.indexOf forms 'where)
        ;; TODO Handle any possible follow-on parts of the query, not just 'where
        select-forms (if (pos? idx-where)
                       (subvec forms 1 idx-where)
                       (subvec forms 1))
        limit (if-let [limit (second (re-find #"select#(\d+)" (name (first forms))))]
                (Long/parseLong limit)
                (count tbl))
        limit-idxs (into [] (range limit))
        where-fn (when (pos? idx-where)
                   (nth forms (inc idx-where)))
        tbl (if where-fn
              (to-table (into []
                              (comp (filter (eval where-fn))
                                    (take limit))
                              (enmap tbl)))
              (flip (tbl [limit-idxs nil])))
        tbl (if (empty? select-forms)
                tbl
                (flip (tbl [(vec (take (min (count tbl) limit) limit-idxs)) select-forms])))
        asc-idx (.indexOf forms 'asc)
        desc-idx (.indexOf forms 'desc)
        sort-it (fn [tbl]
                  (cond
                    (pos? asc-idx) (asc tbl (nth forms (inc asc-idx)))
                    (pos? desc-idx) (desc tbl (nth forms (inc desc-idx)))
                    :else tbl))]
    (sort-it tbl)))

(defn from
  "Limited query capabilities against tables.

  Examples:

  (from example-table
   select name line where added = \"1.9\")
  "
  [table query]
  (interpret-q-forms table query))

(defn zzz
  [l]
  (let [l '(partial map partial map count)]
    ;; TODO Recursive builder of the s-expressions needed.
    ))

(defn upper
  [x]
  (cond
    (string? x)
    (str/upper-case x)

    (coll? x)
    (map upper x)

    :else ::error)
  )

(defn check-ragged
  [x y]
  (let [shape-x (shape x)
        shape-y (shape y)]
    (when (not= shape-x shape-y)
      (throw (ex-info (str "Your collections must be the same rank and shape, "
                           "but x has shape " shape-x " and y has shape " shape-y)
                      {:x x
                       :y y})))))


;; Naive examples of multi-ranked arithmetic functions.
(defn +
  ([] 0)
  ([x] x)
  ([x y]
   (cond
     (and (coll? x) (coll? y)) (do (check-ragged x y)
                                   (map + x y))
     (coll? x) (if (coll? (first x))
                 (map (partial + y) x)
                 (map clojure.core/+ x (repeat y)))
     (coll? y) (if (coll? (first y))
                 (map (partial + x) y)
                 (map clojure.core/+ y (repeat x)))
     :else (clojure.core/+ x y)))
  ([x y & more]
   (reduce + (+ x y) more)))

(defn *
  ([] 1)
  ([x] x)
  ([x y]
   (cond
     (and (coll? x) (coll? y)) (do (check-ragged x y)
                                   (map * x y))
     (coll? x) (if (coll? (first x))
                 (map (partial * y) x)
                 (map clojure.core/* x (repeat y)))
     (coll? y) (if (coll? (first y))
                 (map (partial * x) y)
                 (map clojure.core/* y (repeat x)))
     :else (clojure.core/* x y)))
  ([x y & more]
   (reduce * (* x y) more)))

(defn -
  ([x] (if (coll? x)
         (map clojure.core/- x)
         (clojure.core/- x)))
  ([x y]
   (cond
     (and (coll? x) (coll? y)) (do (check-ragged x y)
                                   (map - x y))
     (coll? x) (if (coll? (first x))
                 (map #(- % y) x)
                 (map clojure.core/- x (repeat y)))
     (coll? y) (if (coll? (first y))
                 (map #(- x %) y)
                 (map clojure.core/- (repeat x) y))
     :else (clojure.core/- x y)))
  ([x y & more]
   (reduce - (- x y) more)))

(defn /
  ([x] (if (coll? x)
         (map (partial clojure.core// 1) x)
         (clojure.core// 1 x)))
  ([x y]
   (cond
     (and (coll? x) (coll? y)) (do (check-ragged x y)
                                   (map / x y))
     (coll? x) (if (coll? (first x))
                 (map (partial / y) x)
                 (map clojure.core// x (repeat y)))
     (coll? y) (if (coll? (first y))
                 (map (partial / x) y)
                 (map clojure.core// y (repeat x)))
     :else (clojure.core// x y)))
  ([x y & more]
   (reduce / (/ x y) more)))

(defn largest [x]
  (if (coll? x)
    (if (empty? x)
      0
      (reduce
       (fn [n y]
         (let [m (largest y)]
           (Math/max n m)))
       0
       x))
    (count (pr-str x))))

(defn list-to-table
  ([aseq] (list-to-table aseq (largest aseq)))
  ([aseq column-width]
   (let [string-writer (java.io.StringWriter.)
         stream (get-pretty-writer string-writer)]
     (binding [*out* stream]
       (doseq [row aseq]
         (doseq [col row]
           (cl-format true "~4D~7,vT" col column-width))
         (prn)))
     (.flush stream)
     (.toString string-writer))))

(def ^:dynamic *table-column-width* nil)

(defn map-to-table [m]
  (let [string-writer (java.io.StringWriter.)
        stream (get-pretty-writer string-writer)
        lk (largest (keys m))]
    (binding [*out* stream]
      (doseq [[k v] (seq m)]
        (print (format (str "%" lk "s |") (pr-str k)))
        (print " ")
        (print (let [s (pr-str v)
                     c (count s)]
                 (if (and *table-column-width*
                          (> c *table-column-width*))
                   (str (apply str (take 30 (pr-str v))) "...")
                   s)))
        (prn)))
    (.flush stream)
    (.toString string-writer)))

;; From Clojure's implementation:
(defn print-table
  "Visually calmer version of clojure.pprint/print-table"
  ([ks rows]
   (when (seq rows)
     (let [widths (map
                   (fn [k]
                     (apply max (count (str k)) (map #(count (str (get % k))) rows)))
                   ks)
           spacers (map #(apply str (repeat % "-")) widths)
           fmts (map #(str "%" % "s") widths)
           fmt-row (fn [leader divider trailer row]
                     (str leader
                          (apply str (interpose divider
                                                (for [[col fmt] (map vector (map #(get row %) ks) fmts)]
                                                  (format fmt (str col)))))
                          trailer))]
       (println)
       #_(println (fmt-row "| " " | " " |" (zipmap ks ks)))
       (println (fmt-row "  " "   " "  " (zipmap ks ks)))
       #_(println (fmt-row "|-" "-+-" "-|" (zipmap ks spacers)))
       (println (fmt-row "--" "- -" "--" (zipmap ks spacers)))
       (doseq [row rows]
         #_(println (fmt-row "| " " | " " |" row))
         (println (fmt-row "  " "   " "  " row))))))
  ([rows]
   (print-table (keys (first rows)) rows)))

(def ^:dynamic *hack* 2)

(defn print-aligned
  ([rows]
   (if-not (coll? (first rows))
     (println (str/join " " rows))
     (doseq [xs rows]
       (if-not (coll? (first xs))
         (let [largest (largest xs)]
           (doseq [x xs]
             ;; Hack for bad alignment at present:
             (print (format (str "%" (inc (max *hack* largest)) "s") x))))
         (print-aligned xs))
       (println))))
  ;; This is superseded by `reshape`
  ([n rows]
   (let [largest (largest rows)]
     (doseq [xs (partition-all n rows)]
       (doseq [x xs]
         (print (format (str "%" (inc largest) "s") x)))
       (println)))))

(defmethod print-method Table
  [t w]
  (let [header (.header t)
        hm-rows (map #(apply hash-map (interleave header %)) (.rows t))]
    (.write w (with-out-str (print-table #_clojure.pprint/print-table header hm-rows)))))

(defmethod print-method ColumnMap
  [cm w]
  (.write w (map-to-table cm)))

(extend-protocol IAmColumnar
  clojure.lang.APersistentVector
  (to-column-map [this]
    (column-map
     (reduce
      (fn [cols row]
        (reduce-kv
         (fn [acc k v]
           (update acc k (fnil conj []) v))
         cols
         row))
      {}
      this)))
  (to-table [this]
    (to-table (to-column-map this)))

  clojure.lang.APersistentMap
  (to-column-map [this]
    (column-map this))
  (to-table [this]
    (to-table (to-column-map this))))

(defn- type-name
  [x]
  (let [raw-name (.getName (type x))
        sym (symbol
             (cond
               (str/starts-with? raw-name "clojure.lang.") (subs raw-name 13)
               (str/starts-with? raw-name "java.lang.")    (subs raw-name 10)
               :else raw-name))]
    (cond
      (symbol? x)
      (try
        (type-name (resolve x))
        (catch Throwable t
          sym))

      (var? x)
      (type-name @x)

      (fn? x)
      'fn

      :else sym)))

(defn print-forms
  [forms walker]
  (let [forms (if (string? forms)
                (read-string forms)
                forms)
        pos (atom 0)
        res (atom {})]
    (walker
     (fn [x]
       (swap! pos inc)
       (swap! res assoc [@pos (type-name x)] x)
       x)
     forms)
    (print-table [(into (sorted-map) @res)])))

(defn forms-top-down
  "Compare with J's facilities for showing the parsed output as boxed words, for
  example:

     ;: '<;._1'
  +-+--+--+
  |<|;.|_1|
  +-+--+--+ "
  [forms]
  (print-forms forms walk/prewalk))

(defn forms-bottom-up
  [forms]
  (print-forms forms walk/postwalk))

(defprotocol IAmOrderable
  (zero [this] "Return the zero item of an orderable collection of values.")
  (succ [this] "Return the this + 1 item of an orderable collection of values.")
  (pred [this] "Return the this - 1 item of an orderable collection of values."))

(extend-protocol IAmOrderable
  Number
  (zero [this] 0)
  (succ [this] (inc this))

  Character
  (zero [this] (char 0))
  (succ [this] ((comp char inc int) this))

  String
  (zero [this] "")
  (succ [this]
    (if-let [l (last this)]
      (str this (succ l))
      "a"))
  (pred [this]
    (let [length (count this)]
      (if (zero? length)
        ""
        (subs this 0 (dec (count this)))))))

;; TODO Support step argument in addition to start, end
(defn value-range
  "Both `start` and `end` are inclusive. Expects `IAmOrderable` values."
  ([start] (iterate succ start))
  ([start end]
   ;; TODO Lazy
   (loop [current start idx 0 res [start]]
     (if (or (= current end)
             (= idx end))
       res
       (let [nxt (succ current)
             nxt-idx (inc idx)]
         (recur nxt
                nxt-idx
                (conj res nxt)))))))

(defn memo-table
  "Table the given `f` over the given `domains` which should be ranges
  of values for each argument to `f`, memoizing the results.

  You can craft your function's domain by hand or reify the
  `IAmOrderable` protocol if your data is amenable to ordering.

  Infinite ranges are acceptable but are cached only up to `limit`.

  Example:

  (def fahrenheit->celsius
       (memo-table (fn [temp] (float (* (- temp 32) (/ 5 9))))
                   (map vector (range 0 500))))"
  ([f domain] (memo-table f domain 1000))
  ([f domain limit]
   (let [memoized (memo/lru f :lru/threshold limit)]
     (doseq [arg (take limit domain)]
       (apply f arg))
     memoized)))

(defn subs'
  "Version of subs that is 2-arity, for use with fn-table"
  [s [start end]]
  (let [cnt (count s)
        end (or end cnt)]
    (subs s start (if (> end cnt)
                    cnt
                    end))))

(defn maybe-name
  [x]
  (or (:name (meta x))
      (str x)))

(defn print-fn-table
  "Render a table of arguments and return values for `f` as a table. The
  function `f` is assumed to take two arguments.

  Examples:
  (print-fn-table #'+ (range -5 5) (range -5 5))
  (print-fn-table #'- (range -5 5) (range -5 5))
  (print-fn-table #'* (range -5 5) (range -5 5))
  (defn safe-div [a b] (if (zero? b) (or (and (pos? a ) ##Inf) ##-Inf) (/ a b)))
  (print-fn-table #'safe-div (range -5 5) (range -5 5))
  "
  [f args-first args-second]
  (let [fn-name (maybe-name f)]
    (let [ks (cons fn-name (map maybe-name args-second))
          rows (map (fn [arg-first]
                      (let [arg-first-name (maybe-name arg-first)]
                        (reduce
                         (fn [acc arg-second]
                           (let [ret (f arg-first arg-second)
                                 ret (if (instance? clojure.lang.LazySeq ret)
                                       (into [] ret)
                                       ret)
                                 arg-second-name (maybe-name arg-second)]
                             (-> acc
                                 (assoc arg-second-name ret)
                                 (assoc fn-name arg-first-name))))
                         {}
                         args-second)))
                    args-first)]
      (print-table ks rows))))

;; TODO ([] eid:1001 1002)#kt returning sub-keyed-table of records 1001, 1002
(defn copy
  "Creates a new collection in which each integer in `x` controls how many
  times the corresponding item of `y` appears.

  If `y` is a map, `x` should be a list of keys to copy into a new map."
  [x y]
  (cond
    (map? y)
    (select-keys y x)

    (coll? x)
    (if (= (count x) 1)
      (let [n (first x)]
        (mapcat #(repeat n %) y))
      (do (check-ragged x y)
          (mapcat #(repeat %1 %2) x y)))

    :else ;; x is atomic value
    (mapcat #(repeat x %) y)))

(comment
  (copy [1 0 1] ['a 'b 'c])
  (copy (map (partial * 2) [1 0 1]) ['a 'b 'c])
  (copy [:alpha :gamma] {:alpha "Alpha" :beta "Beta" :gamma "Gamma"})
  )

(defn- cleanup-docstrings [var]
  (update (meta var)
          :doc
          #(when %
             (clojure.string/replace % #"\n" "  "))))

(defn ns-table []
  (to-table (into [] (sort-by :name
                              (map cleanup-docstrings
                                   (vals (ns-publics 'clojure.core)))))))

(defn- calc-stem
  ([number] (calc-stem 10 number))
  ([by number]
   (int (Math/floor (/ number by)))))

(defn- calc-leaf
  ([number] (calc-leaf 10 number))
  ([by number]
   (mod number by)))

(defn- new-plant
  "Returns a leafless plant, with `size` empty branches,
  i.e. a hash-map with integer keys (from 0 to `size` inclusive)
  mapped to empty vectors.

  (new-plant 2) ;=> {0 [] 1 [] 2 []}"
  [size]
  (let [end (inc size)]
    (->> (repeat end [])
         (interleave (range end))
         (apply hash-map))))

(defn- sprout-leaves
  [plant [stem leaf]]
  (update plant stem conj leaf))

(defn stem-and-leaf
  ([numbers] (stem-and-leaf 10 numbers))
  ([unit numbers]
   (let [calc-stem  (partial calc-stem unit)
         calc-leaf  (partial calc-leaf unit)
         max-stem   (calc-stem (reduce max numbers))
         baby-plant (new-plant max-stem)
         plant      (->> (map (juxt calc-stem calc-leaf) numbers)
                         (reduce sprout-leaves baby-plant)
                         (sort))]
     (doseq [[stem leaves] plant]
       (print   (format (str "%2s") stem))
       (print   " | ")
       (println (clojure.string/join " " (sort leaves)))))))

(comment
  (let [data
        [12 127 28 42 39 113 42 18 44 118 44 37 113 124 37 48 127 36 29 31 125
         139 131 115 105 132 104 123 35 113 122 42 117 119 58 109 23 105 63 27
         44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58 114 126 53 114
         96 25 109 7 31 141 46 13 27 43 117 116 27 7 68 40 31 115 124 42 128 146
         52 71 118 117 38 27 106 33 117 116 111 40 119 47 105 57 122 109 124
         115 43 120 43 27 27 18 28 48 125 107 114 34 133 45 120 30 127 31 116]]
    (stem-and-leaf data)))

(comment
  ;; Initial sketch on keyed tables
  ;; In q, keyed tables are actually dictionaries built by associating a dedicated key table with your main "data" table.
  ;; Fields to add to a KeyedTable type? keyed-on keyed-index
  ([cm] (ColumnMap->Table ::id))
  ([cm keyed-on]
   (let [header  (keys cm)
         rows    (reduce-kv
                  (fn [rs column values]
                    (map (fn [row value] (conj row value)) rs values))
                  (map #(vector %) (range (count (val (first cm)))))
                  cm)
         indexed (if (= ::id keyed-on)
                   (into {}
                         (map-indexed (fn [idx x] [idx x]) rows))
                   (reduce #(assoc %1 keyed-on (get %2 keyed-on)) {} rows))]
     (KeyedTable. header
                  rows
                  cm
                  keyed-on
                  keyed-index)))
  ;; Looking at Clojure namespaces as tables of definitions
  ;; Likely a keyed table a la the `meta` returned by q for a table (its columns)
  (let [ks (keys (meta #'conj))]
    (spit "interns-table.tsv"
          (clojure.string/join \newline
                               (map (fn [var] (clojure.string/join \tab (map var ks)))
                                    (sort-by :name (map (fn [var]
                                                          (update (meta var) :doc (fn [s]
                                                                                    (when s
                                                                                      (clojure.string/replace s #"\n" "  ")))))
                                                        (vals (ns-publics 'clojure.core))))))))

  ;; Table and column-map fun with ns-publics
  (let [cct (to-table (into [] (sort-by :name
                                        (map cleanup-docstrings
                                             (vals (ns-publics 'clojure.core))))))]
    (flip (cct [(vec (range 200 220)) [:name :added]])))

  (let [os   {:windows '
              :mac     '
              :linux   '}
        file "/Users/daniel/tmp/table.html"
        tb   (table {:name   ['Jack 'Jill 'Lindsey 'Paula 'Peter 'Paxton]
                     :age    [5  10 15  20  20  20]
                     :height [40 50 55  60  65  65]
                     :weight [35 50 110 150 200 190]
                     :os     (map #(with-meta % {::display (os (:os %))})
                                  [{:os   :windows
                                    :oss? false}
                                   {:os   :mac
                                    :oss? false}
                                   {:os   :linux
                                    :oss? true}
                                   {:os   :linux
                                    :oss? true}
                                   {:os   :mac
                                    :oss? false}
                                   {:os   :mac
                                    :oss? false}
                                   {:os   :windows
                                    :oss? false}])})]
    (from tb
          '[select where #(str/starts-with? (name (:name %)) "P") desc [:weight :height]]))

;;;;;;;;;;;;;;;;;;;;;
  ;; 1. Presentation ;;
;;;;;;;;;;;;;;;;;;;;;

  '(WARNING (:refer-clojure :exclude [+ - * / count]))

;;;;;;;;;
  ;; let ;;
;;;;;;;;;
  (let [alpha   2
        beta    3
        epsilon 24]
    (+ alpha beta epsilon))

  (let [alpha                          2
        beta                           3
        {{:keys [bamboozled]} :foozle} {:foozle {:bamboozled 42}}
        epsilon                        24]
    (+ alpha beta bamboozled epsilon))

;;;;;;;;;;;;;;;;
  ;; cond-table ;;
;;;;;;;;;;;;;;;;
  (comment
    ;; cond-table
    (require 'com.semperos.rankle.util-test)
    )






































;;;;;;;;;;;;;;;;;;;;;
  ;; 2. Multimethods ;;
;;;;;;;;;;;;;;;;;;;;;

  (declare creation-states)

  (defn create-entity-dispatch
    [[external-state internal-state]]
    (creation-states [external-state internal-state]))

  (defmulti create-entity #'create-entity-dispatch)

  (def creation-states
    (keyed-on
     create-entity
     (value-table
      [[create-entity               ::local-already-exists   ::local-doesnt-exist]
       [::external-already-existed  ::ensure-entities-match  ::create-local-entity]
       [::external-didnt-exist      ::error-entity-mismatch  ::create-local-entity]
       [::external-invalid          ::error-entity-invalid   ::error-entity-invalid]
       [::external-error            ::error-external         ::error-external]])))

  (defmethod create-entity ::error-entity-mismatch
    [_]
    ::maybe-not)

  (create-entity [::external-didnt-exist ::local-already-exists])

  ;; Test for comprehensive defmethods
  (set/difference
   (set (vals creation-states))
   (set (keys (methods create-entity))))

  (comment
    (require 'com.semperos.rankle.org)
    )





































;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Column Maps & Tables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Q & kdb+
  ;; http://code.kx.com/q/

  (comment
    (print-forms '((comp (partial * 2) inc) 20)
                 walk/postwalk)
    )

  (let [data (mapv (fn [idx]
                     {:foo    (* (inc idx) 15)
                      :barcel (nth ["alpha" "beta" "gamma" "delta" "epsilon"] idx)
                      :baz    (mapv #(* (inc idx) 2 %) [1 2 3])})
                   (range 5))
        cm   (to-column-map data)
        t    (to-table cm)]
    (to-column-map data)
    (flip (to-column-map data))
    (row-maps (flip (to-column-map data)))
    (print-table (row-maps (flip (to-column-map data)))))

  (let [os   {:windows '
              :mac     '
              :linux   '}
        file "/Users/daniel/tmp/table.html"
        tb   (table {:name   ['Jack 'Jill 'Lindsey 'Paula 'Peter 'Paxton]
                     :age    [5  10 15  20  25 30]
                     :height [40 50 55  60  65  65]
                     :weight [35 50 110 150 200 190]
                     :os     (map #(with-meta % {::display (os (:os %))})
                                  [{:os   :mac
                                    :oss? false}
                                   {:os   :linux
                                    :oss? true}
                                   {:os   :linux
                                    :oss? true}
                                   {:os   :mac
                                    :oss? false}
                                   {:os   :mac
                                    :oss? false}
                                   {:os   :windows
                                    :oss? false}])})]
    (spit file
          (html/doc
           (html/table
            (from tb
                  '[select :name :age :os
                    where #(str/starts-with? (name (:name %)) "P")
                    desc [:height :weight]
                    ])))))



  (count (ns-table))

  (from (ns-table)
        '[select :name :added :file
          where #(= (:added %) "1.9")])

  (let [data (mapv (fn [idx]
                     {:foo    (* (inc idx) 15)
                      :barcel (nth ["alpha" "beta" "gamma" "delta" "epsilon"] idx)
                      :baz    (mapv #(* (inc idx) 2 %) [1 2 3])})
                   (range 5))
        cm   (to-column-map data)
        t    (to-table cm)]
    (from t
          '[select :baz :barcel
            ;; where #(> (:foo %) 30)
            ;; asc :barcel
            ]))

  (comment
    (let [n 10]
      (print-fn-table #'subs'
                      (repeat n (last (take n (iterate succ "a"))))
                      (for [n (range (inc n))]
                        [0 n])))

    (let [n 10]
      (print-fn-table #'subs'
                      (value-range "a" n)
                      (for [n (range (inc n))]
                        [0 n])))
    )

  ;; CAUTION -- You thought your app booted slowly before...
  (def tabled-subs
    (let [n 10]
      (memo-table subs' (map #(vector %1 %2)
                             (value-range "a" n)
                             (for [n (range (inc n))]
                               [0 n])))))

  (tabled-subs "abcdef" [0 2])

  (comment
    (print-fn-table #'+ (range -4 5) (range -4 5))
    (print-fn-table #'* (range -4 5) (range -4 5))

    #_#_BOOM (print-fn-table #'/ (range -4 5) (range -4 5))

    (defn safe-div [a b] (if (zero? b) (or (and (pos? a ) ##Inf) ##-Inf) (/ a b)))
    (print-fn-table #'safe-div (range -5 5) (range -5 5))
    )














































;;;;;;;;;;;;;;;;;;;;;;;
  ;; Array-Programming ;;
;;;;;;;;;;;;;;;;;;;;;;;

  ;; J & Jd
  ;; https://code.jsoftware.com/wiki/NuVoc

  (comment
    (let [coll [1 2 3]
          xs   (map #(map % [1 2 3])
                    [identity inc (comp inc inc)])]
      (print-aligned xs))
    )

  (reshape [3 4] (range 1 13))

  (comment
    (print-aligned (reshape [3 4] (range 1 13)))
    1  2  3  4
    5  6  7  8
    9 10 11 12
    )

  (let [coll (reshape [3 4] (range 1 13))]
    (count coll)
    ((rank count 1) coll)
    )

  (comment
    (print-aligned (reshape [3 4 2] (range 1 25)))
    )

  ;; J

  (let [coll (reshape [3 4 2] (range 1 25))]
    (count coll)
    ((rank count 1) coll)
    ((rank count 2) coll)
    )

  (+ 1 2)
  (+ 1 [1 2 3])
  (+ [1 2 3] [5 6 7])

  (comment
    (let [coll (reshape [3 3] (range 9))]
      (print-aligned (+ coll coll))
      ;; (print-aligned (+ coll (reshape [3 4] (range 12))))
      )
    )

  (comment
    (let [coll (reshape [3 3] (range 9))
          f    *
          ;; f -
          ]
      (print-aligned (f coll coll)))
    )

  (upper "foo")
  (upper ["foo" "bar" "baz"])

  (comment
    (print-aligned
     (upper (reshape [3 4] (take 12 (cycle ["foo" "bar" "baz"])))))

    (print-aligned
     (upper (reshape [3 4] (take 12 (cycle ["foozle" "bar" "baz"])))))
    )

  (let [s "deadly"]
    (reduce str
            (copy [1 1 1 1 1 1] s)
            ;; (copy [1 0 1 1 0 0] s)
            ;; (copy [1 0 1 2 0 1] s)
            )
    )












  :Tables-Considered-Helpful



  )
