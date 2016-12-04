(ns blog.main.user-editor
  (:require [reagent.core :as r]
            [blog.util :refer [value-of]]
            [re-frame.core :refer [subscribe dispatch]]
            blog.state.user-editor-subs
            blog.state.user-editor-handlers))

(defn user-editor []
  (let [user (subscribe [:current-user])
        user-model (r/atom (-> @user
                               (assoc :password "")
                               (dissoc :userid
                                       :primary-group-name
                                       :permissions)))]
    (fn []
      [:div#form
       [:label "Nickname"] [:input {:type :text
                                    :value (:nickname @user-model)
                                    :on-change #(swap! user-model assoc :nickname (value-of %))}]
       [:label "Avatar's url"] [:input {:type :text
                                        :value (:img_location @user-model)
                                        :on-change #(swap! user-model assoc :img_location (value-of %))}]
       [:label "Password"] [:input {:type :password
                                    :value (:password @user-model)
                                    :on-change #(swap! user-model assoc :password (value-of %))}]
       [:button {:on-click #(dispatch [:save-user @user-model])} "Submit"]])))
