(ns com.semperos.rankle.org
  "Tables from org-mode"
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [dorothy.core :as dot]
            [dorothy.jvm :as dj]
            [fsm-clj.core :refer [defsm send-event show!]]
            [hiccup.core :as h]
            [hiccup.page :as page]))

(defn kw
  [s]
  (keyword (string/replace s #"\s" "_")))

(defn format-events
  [events]
  (mapv
   (fn [event]
     (let [form (read-string (str "[" event "]"))
           [event params] form]
       (if params
         (kw event) #_TODO-handle-params
         (kw event))
       (keyword event)))
   events))

(defn format-transition
  [state legend]
  (fn [[event transition]]
    (when-not (empty? transition)
      (let [[conditions next-state] (string/split transition #"\s*->\s*")
            next-state (kw next-state)
            conditions (read-string conditions)
            abbrev (get legend event)]
        (if (empty? conditions)
          [state '-> next-state 'when abbrev]
          [state '-> next-state 'when abbrev #_TODO])))))

(defn event-legend [events]
  (let [start 97
        chars (map (comp keyword str char) (range start (+ start 26)))]
    (apply hash-map (interleave events chars))))

(defn table-data
  [org-str]
  (let [[events _ & rows]
        (map (fn [row]
               (string/split row #"\s*\|\s*"))
             (string/split org-str #"\n"))]
    [(format-events (nnext events)) (map next rows)]))

(defn table-to-state-machine
  [org-str]
  (let [[events rows] (table-data org-str)
        event-legend (event-legend events)]
    (into []
          (comp (mapcat (fn mc [row]
                          (let [state (kw (first row))
                                events (apply hash-map (interleave events (next row)))]
                            (into []
                                  (comp (map (format-transition state event-legend))
                                        (keep identity))
                                  events))))
                (distinct))
          rows)))

(defn state-machine-legend
  [org-str]
  (event-legend (first (table-data org-str))))

(defn legend-table
  [legend]
  [:table
   [:thead
    [:tr
     [:th "Abbrev."] [:th "Event"]]]
   [:tbody
    (for [[e a] (sort-by val legend)]
      [:tr [:td [:code a]] [:td [:code e]]])]])

(defmacro deftablesm
  "Macro to define a state machine with fsm-clj from an org-mode table."
  [name resource-name]
  (let [s (slurp (io/resource resource-name))
        v (table-to-state-machine s)]
    `(defsm ~name ~v)))

(defn table-to-html
  [org-str]
  (let [[events _ & rows]
        (map (fn [row]
               ;; Different regex
               (string/split row #"\|"))
             (string/split org-str #"\n"))]
    (h/html
     [:table
      [:thead
       [:tr (for [event events]
              [:th [:code event]])]]
      [:tbody
       (for [[idx row] (map-indexed #(vector %1 %2) rows)]
         [:tr {:class (["even" "odd"] (mod idx 2))}
          (for [cell row]
            [:td [:code cell]])])]])))

(deftablesm pres "presentation-sm.org")

(defn save-svg!
  [fsm out-file]
  (-> fsm :graph
      dot/digraph dot/dot
      (dj/save! out-file
                {:format :svg})))

(defn updated-at []
  (str "Updated " (.format (java.time.LocalTime/now) java.time.format.DateTimeFormatter/ISO_LOCAL_TIME)))

(def page-styles
  "
  table { margin: 0 0 50px 0; }
  table tr.even { background-color: #efefef; }
  table td { padding: 0 5px; }
  div.graph { float: left; }
  .side-note { float: right; font-size: 0.8em; font-style: italic; }
  ")

(defn save-page!
  [org-str fsm]
  (save-svg! fsm "/Users/daniel/tmp/states.svg")
  (spit "/Users/daniel/tmp/states.html"
        (page/html5
         [:head
          [:style page-styles]]
         [:body
          [:div.side-note
           (updated-at)]
          [:div
           (table-to-html org-str)]
          [:div.graph
           [:img {:src "states.svg"}]]
          [:div
           (legend-table (state-machine-legend org-str))]])))

(save-page! (slurp (io/resource "presentation-sm.org")) (pres))

(comment
  (table-to-state-machine (slurp "/Users/daniel/tmp/states.org"))
  )
