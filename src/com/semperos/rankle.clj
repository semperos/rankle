(ns com.semperos.rankle
  (:refer-clojure :exclude [+ - * / > < >= <= count])
  (:require [clojure.core.matrix :as mx]
            [clojure.core.memoize :as memo]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [com.semperos.rankle.html :as html]
            [com.semperos.rankle.util :refer [cond-table print-table] :as util])
  (:import [java.io Writer]))

(defn ^{:rank ##Inf}
  count
  [coll]
  (clojure.core/count coll))

(defn shape
  [x]
  (or (mx/shape x) []))

(defn reshape
  [shape coll]
  {:pre [(every? number? shape)]}
  (mx/reshape coll shape))

(defn rank
  ([x]
   (if (var? x)
     ((comp :rank meta) x)
     (count (shape x))))
  ([f r]
   (fn rankle
     ([arg0]
      (if (clojure.core/<= (rank arg0) r)
        (f arg0)
        (map rankle arg0)))
     ([arg0 arg1]
      (if (clojure.core/<= (rank arg1) r)
        (f arg0 arg1)
        (map #(f arg0 %) arg1))))))

(defn- wrap [y] (if (seqable? y) y [y]))

(defn over
  "J's /

  Monadic - u Insert y
  Dyadic  - x u Table y"
  [f]
  (fn overly
    ([coll]
     (reduce (fn [acc x]
               (f acc x))
             coll))
    ([x y]
     (let [res (for [x (wrap x)]
                 (for [y (wrap y)]
                   (f x y)))]
       (if (and (seqable? x)
                (seqable? y))
         res
         (first res))))))

(defn check-ragged
  [x y]
  (let [shape-x (shape x)
        shape-y (shape y)
        cx (count shape-x)
        cy (count shape-y)
        min' (min cx cy)]
    (when-not (= (take min' shape-x)
                 (take min' shape-y))
      (throw (ex-info (str "The shape of the lower-ranked argument must "
                           "match the common frame of the shape of the higher "
                           "ranked argument , but x had shape " shape-x
                           "and y had shape " shape-y)
                      {:x shape-x
                       :y shape-y})))))


;; Examples of multi-ranked arithmetic functions.
(defn +
  ([] 0)
  ([x] x)
  ([x y]
   (check-ragged x y)
   (cond
     (and (seqable? x) (seqable? y)) (map + x y)
     (seqable? x) (map (rank (partial clojure.core/+ y) 0) x)
     (seqable? y) (map (rank (partial clojure.core/+ x) 0) y)
     :else (clojure.core/+ x y)))
  ([x y & more]
   (reduce + (+ x y) more)))

(defn *
  ([] 1)
  ([x] x)
  ([x y]
   (check-ragged x y)
   (cond
     (and (seqable? x) (seqable? y)) (map * x y)
     (seqable? x) (map (rank (partial clojure.core/* y) 0) x)
     (seqable? y) (map (rank (partial clojure.core/* x) 0) y)
     :else (clojure.core/* x y)))
  ([x y & more]
   (reduce * (* x y) more)))

(defn -
  ([x] (if (seqable? x)
         ((rank clojure.core/- 0) x)
         (clojure.core/- x)))
  ([x y]
   (check-ragged x y)
   (cond
     (and (seqable? x) (seqable? y)) (map - x y)
     (seqable? x) (map (rank #(clojure.core/- % y) 0) x)
     (seqable? y) (map (rank (partial clojure.core/- x) 0) y)
     :else (clojure.core/- x y)))
  ([x y & more]
   (reduce - (- x y) more)))

(defn /
  ([x] (if (seqable? x)
         ((rank (partial clojure.core// 1) 0) x)
         (clojure.core// 1 x)))
  ([x y]
   (check-ragged x y)
   (cond
     (and (seqable? x) (seqable? y)) (map / x y)
     (seqable? x) (map (rank #(clojure.core// % y) 0) x)
     (seqable? y) (map (rank (partial clojure.core// x) 0) y)
     :else (clojure.core// x y)))
  ([x y & more]
   (reduce / (/ x y) more)))

(defn >
  ([x] (if (seqable? x)
         ((rank > 0) x)
         1))
  ([x y]
   (check-ragged x y)
   (cond
     (and (seqable? x) (seqable? y)) (map > x y)
     (seqable? x) (map (rank #(> % y) 0) x)
     (seqable? y) (map (rank (partial > x) 0) y)
     :else (if (clojure.core/> x y) 1 0)))
  ([x y & more]
   (reduce > (> x y) more)))

(defn >=
  ([x] (if (seqable? x)
         ((rank >= 0) x)
         1))
  ([x y]
   (check-ragged x y)
   (cond
     (and (seqable? x) (seqable? y)) (map >= x y)
     (seqable? x) (map (rank #(>= % y) 0) x)
     (seqable? y) (map (rank (partial >= x) 0) y)
     :else (if (clojure.core/>= x y) 1 0)))
  ([x y & more]
   (reduce >= (>= x y) more)))

(defn <
  ([x] (if (seqable? x)
         ((rank < 0) x)
         1))
  ([x y]
   (check-ragged x y)
   (cond
     (and (seqable? x) (seqable? y)) (map < x y)
     (seqable? x) (map (rank #(< % y) 0) x)
     (seqable? y) (map (rank (partial < x) 0) y)
     :else (if (clojure.core/< x y) 1 0)))
  ([x y & more]
   (reduce < (< x y) more)))

(defn <=
  ([x] (if (seqable? x)
         ((rank <= 0) x)
         1))
  ([x y]
   (check-ragged x y)
   (cond
     (and (seqable? x) (seqable? y)) (map <= x y)
     (seqable? x) (map (rank #(<= % y) 0) x)
     (seqable? y) (map (rank (partial <= x) 0) y)
     :else (if (clojure.core/<= x y) 1 0)))
  ([x y & more]
   (reduce <= (<= x y) more)))

(defprotocol IIndexable
  (index-of [this x] "Return 0-based index of `x` in `this` or the length of `this` if `x` is not present."))

(extend-protocol IIndexable
  clojure.lang.Indexed
  (index-of [this x]
    (let [idx (.indexOf this x)]
      (if (= idx -1)
        (count this)
        idx)))

  String
  (index-of [this x]
    (let [idx (.indexOf this (str x))]
      (if (= idx -1)
        (count this)
        idx)))

  Object
  (index-of [this x]
    (let [idx (.indxOf this x)]
      (if (= idx -1)
        (count this)
        idx))))

;; TODO Consider fill
(defn ravel
  "J's ,

  Monadic - Ravel y
  Dyadic  - x Append y

  WARNING Presently this does _not_ fill arrays, so ragged collections
  will remain so."
  ([y]
   (flatten y))
  ([x y]
   (cond
     (and (seqable? x) (seqable? y)) (concat x y)
     (seqable? x) (ravel x [y])
     (seqable? y) (ravel [x] y)
     :else (ravel [x] [y]))))

(defn in
  "J's i."
  ([n]
   (cond
     (number? n) (range n)
     (and (vector? n) (every? number? n)) (reshape n (range (reduce * 1 n)))
     :else (throw (IllegalArgumentException. "For 1-arity in, you must supply either a number or a vector of numbers."))))
  ([x y]
   (if (seqable? y)
     (map (partial in x) y)
     (index-of x y))))

(defn from
  "J's left-curly"
  [x y]
  (if (seqable? x)
    (map #(from % y) x)
    (nth y x)))

(def alphabet
  (map char (range 256)))

;; TODO base, antibase https://code.jsoftware.com/wiki/Vocabulary/numberdot
(defn ?
  "J's ?.

  Monadic - Roll y
  Dyadic  - x Deal y"
  ([y]
   (if (seqable? y)
     (map ? y)
     (rand-int y)))
  ([x y]
   (let [ys (in y)]
     (repeatedly x #(from (rand-int y) ys)))))

(defn unicode
  [y]
  (if (seqable? y)
    (map unicode y)
    (first (Character/toChars y))))

;;;;;;;;;;;;;;
;; Printing ;;
;;;;;;;;;;;;;;
(def ^:dynamic *hack* 2)

(defn print-aligned
  ([rows]
   (if-not (coll? (first rows))
     (println (str/join " " rows))
     (doseq [xs rows]
       (if-not (coll? (first xs))
         (let [largest (util/largest xs)]
           (doseq [x xs]
             ;; Hack for bad alignment at present:
             (print (format (str "%" (inc (max *hack* largest)) "s") x))))
         (print-aligned xs))
       (println))))
  ;; This is superseded by `reshape`
  ([n rows]
   (let [largest (util/largest rows)]
     (doseq [xs (partition-all n rows)]
       (doseq [x xs]
         (print (format (str "%" (inc largest) "s") x)))
       (println)))))

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

(defn upper
  [x]
  (cond
    (string? x)
    (str/upper-case x)

    (coll? x)
    (map upper x)

    :else ::error)
  )

(comment

  ;;;;;;;;;;;;;;;;;;;;;
  ;; 1. Presentation ;;
  ;;;;;;;;;;;;;;;;;;;;;

  '(WARNING (:refer-clojure :exclude [+ - * / count]))

  (require '[com.semperos.rankle.table :refer :all])

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

  (comment
    (print-forms '((comp (partial * 2) inc) 20)
                 walk/postwalk)
    )

  ;; Q & kdb+
  ;; http://code.kx.com/q/

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

  (defn- cleanup-docstrings [var]
    (update (meta var)
            :doc
            #(when %
               (clojure.string/replace % #"\n" "  "))))

  (defn ns-table []
    (to-table (into [] (sort-by :name
                                (map cleanup-docstrings
                                     (vals (ns-publics 'clojure.core)))))))

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

  (let [coll #_print-aligned (reshape [3 4 2] (range 1 25))]
    (count coll)
    ;; ((rank count 1) coll)
    ;; ((rank count 2) coll)
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
    (reduce str (copy [1 1 1 1 1 1] s))
    ;; (reduce str (copy [1 0 1 1 0 0] s))
    ;; (reduce str (copy [1 0 1 2 0 1] s))
    )


  :Tables-Considered-Helpful


  )
