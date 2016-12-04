(ns blog.state.register-handlers
  (:require [re-frame.core :refer [dispatch reg-event-db reg-event-fx trim-v]]))

(reg-event-fx
 :register-user
 [trim-v]
 (fn [{:keys [db]} [new-user]]
   {:db db
    :put {:url (str "/api/users/save")
          :body new-user
          :dispatch-key :user-registered}}))

(reg-event-fx
 :user-registered
 [trim-v]
 (fn [{:keys [db]} _]
   {:db db
    :alert "User registered!"
    :redirect-to "/blog/"}))
