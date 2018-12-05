(ns com.semperos.rankle.table
  (:require [clojure.pprint :refer [get-pretty-writer]]
            [clojure.set :as set]
            [clojure.string :as str]
            [com.semperos.rankle.util :refer [cond-table print-table] :as util]))

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

(def ^:dynamic *table-column-width* nil)

(defn map-to-table [m]
  (let [string-writer (java.io.StringWriter.)
        stream (get-pretty-writer string-writer)
        lk (util/largest (keys m))]
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
  )
