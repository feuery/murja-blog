(ns blog.state.subs
  (:require [re-frame.core :refer [reg-sub]]))

(reg-sub :settings
         (fn [db _]
           (or (get db :settings)
               {})))

(reg-sub :grouper-data
         (fn [db _]
           (or (get db :grouper-titles)
               [])))
