(ns blog.state.user-editor-handlers
  (:require [re-frame.core :refer [dispatch reg-event-db reg-event-fx trim-v]]))

(reg-event-fx
 :save-user
 [trim-v]
 (fn [{:keys [db]} [user-model]]
   {:post {:url (str "/api/users/save")
           :body user-model
           :dispatch-key :user-saved}
    :db db}))

(reg-event-fx
 :user-saved
 [trim-v]
 (fn [{:keys [db]} [{:keys [nickname img_location] :as new-user}]]
   {:db
    (-> db
        (assoc-in [:current-user :nickname] nickname)
        (assoc-in [:current-user :img_location] img_location))
    :alert "User updated!"
    :redirect-to "/blog/"}))

