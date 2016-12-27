(ns blog.state.handlers
  (:require [re-frame.core :refer [dispatch reg-event-db reg-event-fx trim-v]]
            [blog.state.effects]))

;; https://github.com/Day8/re-frame/blob/master/docs/Interceptors.md#wrapping-handlers

(reg-event-fx
 :is-empty?
 (fn [{:keys [db]} _]
   {:get {:url "/api/users/is-empty"
          :dispatch-key :emptiness-found}
    :db db}))

(reg-event-db
 :emptiness-found
 [trim-v]
 (fn [db [is-empty?]]
   (assoc db :is-empty? is-empty?)))

(reg-event-fx
 :load-page
 [trim-v]
 (fn [{:keys [db]} [page page-size]]
   {:get {:url (str "/api/posts/page/" page "/page-size/" page-size)
          :dispatch-key :page-loaded}
    :db (assoc db :page-nr page)}))

(reg-event-fx
 :load-settings
 [trim-v]
 (fn [{:keys [db]} [nr]]
   {:db (assoc db :page-nr nr)
    :get {:url "/api/settings/client-settings"
          :dispatch-key :settings-loaded}}))

(reg-event-db :settings-loaded
              [trim-v]
              (fn [{:keys [page-nr] :as db} [settings]]
                (dispatch [:load-page page-nr (:recent-post-count settings)])
                (-> db
                    (assoc :settings settings)
                    (dissoc :page-nr))))

(reg-event-db
 :page-loaded
 [trim-v]
 (fn [{:keys [page-nr] :as db} [result]]
   (-> db
       (dissoc :page-nr)
       (assoc :page {:posts (:posts result)
                     :nr page-nr}
              :show-devtool? false
              :last-page? (:last-page? result)))))

