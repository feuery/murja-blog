(ns blog.state.post-subs
  (:require [re-frame.core :refer [reg-sub]]))

(reg-sub :current-page
         (fn [db _]
           (or (get db :page)
               [])))

(reg-sub :page-nr
         (fn [db _]
           (get-in db [:page :nr])))
