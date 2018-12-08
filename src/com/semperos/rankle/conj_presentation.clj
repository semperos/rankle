(ns com.semperos.rankle.conj-presentation
  "Code from Clojure Conj 2018 presentation \"Tables Considered Helpful\".")

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
