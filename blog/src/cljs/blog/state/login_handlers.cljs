(ns blog.state.login-handlers
  (:require [re-frame.core :refer [dispatch reg-event-db reg-event-fx trim-v]]
            [blog.state.effects]
            [schema.core :as s]

            [blog.login.schemas :refer [login-response]]))

(reg-event-fx
 :log-in
 [trim-v]
 (fn [{:keys [db]} [login-model]]
   {:post {:url (str "/api/login/login")
           :body login-model
           :dispatch-key :logged-in}
    :db db}))

(reg-event-db
 :logged-in
 [trim-v]
 (fn [db [result]]
   (if (s/validate login-response result)
     (assoc db :current-user result)
     db)))
