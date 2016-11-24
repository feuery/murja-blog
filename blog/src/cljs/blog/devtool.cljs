(ns blog.devtool
  (:require [re-frame.core :refer [dispatch]]
            [blog.state.dev-handlers]
            [blog.util :refer [value-of]]))

(declare devtool)

(defn devtool-branch [path [key val]]
  [:tr
   [:td (pr-str key)]
   [:td (cond (map? val) (into [:table]
                               (mapv (partial devtool-branch (conj path key)) val))

              (vector? val) (into [:table]
                                  (mapv (partial devtool-branch (conj path key)) (zipmap (range) val)))

              :t (let [final-path (conj path key)
                       type (type val)]
                   [:input {:title (pr-str final-path)
                            :default-value (pr-str val)
                            :on-change #(dispatch [:set-db final-path (value-of %) type])}]))]])

(defn devtool [db visible?]
  (if visible?
    [:div#devtool

     (into [:table]
           (mapv (partial devtool-branch []) db))
     
     [:button {:on-click #(dispatch [:toggle-devtool-visibility])}
      "Click to hide appdb's state"]]
    [:div#minidevtool
     [:button
      {:on-click #(dispatch [:toggle-devtool-visibility])}
      "Click to show appdb's state"]]))
