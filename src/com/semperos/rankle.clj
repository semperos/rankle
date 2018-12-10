(ns com.semperos.rankle
  (:refer-clojure :exclude [= + - * / > < >= <= count])
  (:require [clojure.core.matrix :as mx]
            [clojure.core.memoize :as memo]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
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

(defn prefixes [coll]
  (if (empty? coll)
    coll
    (map (fn [n] (take n coll)) (+ 1 (in (count coll))))))

(defn prefix
  "J's \\ adverb.

  Monadic - u Prefix y
  Dyadic  - x u Infix y"
  [f]
  (fn prefixly
    ([coll]
     (map f (prefixes (vec coll))))))

(defn check-ragged
  [x y]
  (let [shape-x (shape x)
        shape-y (shape y)
        cx (count shape-x)
        cy (count shape-y)
        min' (min cx cy)]
    (when-not (clojure.core/= (take min' shape-x)
                              (take min' shape-y))
      (throw (ex-info (str "The shape of the lower-ranked argument must "
                           "match the common frame of the shape of the higher "
                           "ranked argument , but x had shape " shape-x
                           "and y had shape " shape-y)
                      {:x shape-x
                       :y shape-y})))))

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

(defn =
  ([x] (if (seqable? x)
         ((rank = 0) x)
         1))
  ([x y]
   (check-ragged x y)
   (cond
     (and (seqable? x) (seqable? y)) (map = x y)
     (seqable? x) (map (rank #(= % y) 0) x)
     (seqable? y) (map (rank (partial = x) 0) y)
     :else (if (clojure.core/= x y) 1 0)))
  ([x y & more]
   (reduce = (= x y) more)))

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
      (if (clojure.core/= idx -1)
        (count this)
        idx)))

  String
  (index-of [this x]
    (let [idx (.indexOf this (str x))]
      (if (clojure.core/= idx -1)
        (count this)
        idx)))

  Object
  (index-of [this x]
    (let [idx (.indxOf this x)]
      (if (clojure.core/= idx -1)
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
  (mapv char (range 256)))

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
     (if (or (clojure.core/= current end)
             (clojure.core/= idx end))
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
    (if (clojure.core/= (count x) 1)
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

    :else ::error))
