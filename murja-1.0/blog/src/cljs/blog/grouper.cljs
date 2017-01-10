(ns blog.grouper
  (:require [re-frame.core :refer [subscribe dispatch]]
            [blog.date-schemas :refer [int->month-str]]))

(defn grouper [data]
  [:div#grouper "Posts: "
   (into [:ul]
         (map (fn [[year month-map]]
                [:li [:details [:summary (str year)]
                      (into [:ul]
                            (map (fn [[month titles]]
                                   [:li [:details [:summary (str (int->month-str month))]
                                         (into [:ul.title-list]
                                               (map (fn [{:keys [Title Id]}]
                                                      [:li.title-list [:a {:href (str "/blog/post/" Id)} (str Title)]]) titles))]]) month-map))]]) data))])

(defn def-grouper []
  (let [data (subscribe [:grouper-data])]
    (fn []
      [grouper @data])))
