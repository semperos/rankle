(ns com.semperos.rankle.html
  "HTML utilities for tables."
  (:require [hiccup.core :refer [html]]
            [hiccup.page :as page]))

(defn table
  [table]
  (let [header (.header table)
        rows (.rows table)]
    (html
     [:table
      [:thead
       [:tr
        (for [h header] [:th {:style "border-bottom: 1px solid black;"} (pr-str h)])]]
      [:tbody
       (for [r rows]
         [:tr (for [d r
                    :let [m (meta d)]]
                [:td (if-let [display (or (::display m)
                                          (:com.semperos.rankle/display m))]
                       display
                       (pr-str d))])])]])))

(defn doc
  "HTML document."
  [& parts]
  (html
   {:mode :html}
   (page/doctype :html5)
   [:html
    [:head
     [:meta {:http-equiv "refresh"
             :content "1"}]]
    [:body {:style "font-family: 'Hack Nerd Font';"}
     parts]]))
