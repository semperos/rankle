(ns com.semperos.rankle.deprecated)

(comment
  ;; From Clojure's implementation, since private:
  (defn pr-on
    {:private true
     :static true}
    [x w]
    (if *print-dup*
      (print-dup x w)
      (print-method x w))
    nil)

  (defn- print-meta [o, ^Writer w]
    (when-let [m (meta o)]
      (when (and (pos? (count m))
                 (or *print-dup*
                     (and *print-meta* *print-readably*)))
        (.write w "^")
        (if (and (= (count m) 1) (:tag m))
          (pr-on (:tag m) w)
          (pr-on m w))
        (.write w " "))))

  (def core-vector-print-method (get (methods print-method) clojure.lang.IPersistentVector))
  (def core-iseq-print-method (get (methods print-method) clojure.lang.ISeq))
  (def core-map-print-method (get (methods print-method) clojure.lang.IPersistentMap))

  (defmacro override-core-vector-print-method []
    `(defmethod print-method clojure.lang.IPersistentVector
       [v# w#]
       (print-meta v# w#)
       (if (>= (rank v#) 2)
         (.write w# (with-out-str (list-to-table v#)))
         (do
           (doseq [x# v#]
             (.write w# (str x#))
             (.write w# " "))
           (.write w# "\n")))))
  (comment
    (override-core-vector-print-method)
    )

  (defmacro restore-core-vector-print-method []
    `(defmethod print-method clojure.lang.IPersistentVector
       [v# w#]
       (core-vector-print-method v# w#)))

  (defmacro override-core-map-print-method []
    `(defmethod print-method clojure.lang.IPersistentMap
       [m# w#]
       (print-meta m# w#)
       (if (>= (rank v#) 2)
         (.write w# (list-to-table v#))
         (do
           (doseq [x# v#]
             (.write w# (str x#))
             (.write w# " "))
           (.write w# "\n")))))

  (defmacro restore-core-map-print-method []
    `(defmethod print-method clojure.lang.IPersistentMap
       [m# w#]
       (core-map-print-method m# w#))))
