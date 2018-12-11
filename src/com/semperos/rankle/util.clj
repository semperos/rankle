(ns com.semperos.rankle.util)

;; From https://github.com/sritchie/jackknife/blob/master/src/jackknife/def.clj#L4
;; License: Apache License, Version 2.0
(defmacro defalias
  "Defines an alias for a var: a new var with the same root binding (if
  any) and similar metadata. The metadata of the alias is its initial
  metadata (as provided by def) merged into the metadata of the original."
  ([name orig]
   `(do
      (alter-meta!
       (if (.hasRoot (var ~orig))
         (def ~name (.getRawRoot (var ~orig)))
         (def ~name))
       ;; When copying metadata, disregard {:macro false}.
       ;; Workaround for http://www.assembla.com/spaces/clojure/tickets/273
       #(conj (dissoc % :macro)
              (apply dissoc (meta (var ~orig)) (remove #{:macro} (keys %)))))
      (var ~name)))
  ([name orig doc]
   (list `defalias (with-meta name (assoc (meta name) :doc doc)) orig)))

(defn largest [x]
  (if (seqable? x)
    (if (empty? x)
      0
      (reduce
       (fn [n y]
         (let [m (largest y)]
           (Math/max n m)))
       0
       x))
    (count (pr-str x))))

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

(defn validate-cond-table
  "Validate the arguments passed to the `cond-table` macro and return
  the data of the table rows."
  [items]
  (let [rs (into []
                 (comp (partition-by (partial = :|))
                       (partition-all 2))
                 items)
        _ (when-not (every? #(= '(:|) (first %)) rs)
            (throw (IllegalArgumentException. "Each row in cond-table must begin with the keyword :|")))
        rows (map second rs) ;; remove :| syntax
        header-count (count (first rows))
        next-row-counts (into #{} (map count) (next rows))
        next-rows-same-count? (= 1 (count next-row-counts))
        ;; First row with blank first cell, for default `and` behavior
        default-header-validates? (= (inc header-count) (first next-row-counts))
        ;; First row with custom op in first cell
        op-header-validates? (= header-count (first next-row-counts))
        ;; All rows after the first must be same length and first row is either
        ;; the same length (because a custom op was supplied) or has one item
        ;; fewer (default of `and` is being leveraged).
        _ (when-not (and next-rows-same-count?
                         (or default-header-validates?
                             op-header-validates?))
            (throw (IllegalArgumentException. "Every row after the first in cond-table must start with a predicate and include an expression for each cell in the table.")))]
    rows))

(defmacro cond-table
  "Produce a `cond` expression from a tabular representation of its clauses.

  When multiple predicates need to be checked in concert, a tabular
  representation enforces comprehensive handling of all possible
  combinations.

  The first row consists of the right-hand predicates and the first
  column consists of the left-hand predicates. By default, these
  predicates are `and`ed. As the first item of the first row, you can
  supply either a function or macro to use instead of `and`, a symbol
  consisting of underscores to visually enforce column alignment, or
  leave it blank.

  Each row must begin with the keyword `:|`. Note `:|` is an ordinary
  keyword.

  Example:

      (cond-table
        :|             in-progress?   final-run?
        :| succeeded?  (succeed)      (succeed)
        :|    failed?  (retry)        (terminate))

  Which macro-expands to:

      (cond
        (and succeeded? in-progress?) (succeed)
        (and succeeded? final-run?)   (succeed)
        (and failed?    in-progress?) (retry)
        (and failed?    final-run?)   (terminate))"
  [& items]
  (let [rows (validate-cond-table items)
        rights (first rows)  ;; get right-hand conditions
        rights (if (and (symbol? (first rights))
                        (every? (partial = \_) (name (first rights))))
                 (next rights)
                 rights)
        op-omitted? (= (count (second rows))
                       (inc (count rights)))
        [op rights] (if op-omitted?
                      ['and rights]
                      [(first rights) (next rights)])]
    (cons 'cond
          (mapcat
           (fn [[left-condition & exprs :as row]]
             (mapcat
              (fn [right-condition expr]
                ;; `cond` test/expr pair:
                (list (list op left-condition right-condition) expr))
              rights exprs))
           (next rows)))))
