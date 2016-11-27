(ns blog.state.handlers
  (:require [re-frame.core :refer [dispatch reg-event-db reg-event-fx trim-v]]
            [blog.state.effects]))

;; https://github.com/Day8/re-frame/blob/master/docs/Interceptors.md#wrapping-handlers

(reg-event-fx
 :load-page
 [trim-v]
 (fn [{:keys [db]} [page page-size]]
   {:get {:url (str "/api/posts/page/" page "/page-size/" page-size)
          :dispatch-key :page-loaded}
    :db db}))

(reg-event-db
 :page-loaded
 [trim-v]
 (fn [db [result]]
   (assoc db
          :page {:type :recent-posts
                 :posts result}
          :show-devtool? false))) 
