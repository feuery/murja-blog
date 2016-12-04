(ns blog.main.register-view
  (:require [reagent.core :as r]
            [blog.util :refer [value-of]]
            [re-frame.core :refer [subscribe dispatch]]
            blog.state.register-handlers))

(defn register []
  (let [register-form-model (r/atom {})]
    (fn []
      [:div#registration-form
       [:label "Username: "] [:input {:type :text
                                      :value (:username @register-form-model)
                                      :on-change #(swap! register-form-model assoc :username (value-of %))}]
       [:label "Nickname: "] [:input {:type :text
                                      :value (:nickname @register-form-model)
                                      :on-change #(swap! register-form-model assoc :nickname (value-of %))}]
       [:label "URL of your avatar"] [:input {:type :text
                                      :value (:img_location @register-form-model)
                                              :on-change #(swap! register-form-model assoc :img_location (value-of %))}]
       [:label "Password: "] [:input {:type :password
                                      :value (:password @register-form-model)
                                      :on-change #(swap! register-form-model assoc :password (value-of %))}]

       [:button {:on-click #(dispatch [:register-user @register-form-model])} "Register!"]])))
