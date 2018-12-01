(ns com.semperos.rankle.ns-table
  "Tables manifest as namespaces.")

(defn ns-columns [ns]
  (sort (fnil ::column-order Integer/MAX_VALUE)
        (filter (comp ::column meta) (vals (ns-interns ns)))))

(deftype NsDb [ns])

(defprotocol ITable
  (columns [this] "Columns of the table."))

(deftype NsTable [ns]
  ITable
  (columns [this]
    (ns-columns ns)))

(defn table
  [ns]
  (->NsTable ns))


(comment
  (create-ns 'com.semperos.example-table)

  )
