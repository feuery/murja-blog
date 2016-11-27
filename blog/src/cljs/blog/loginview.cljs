(ns blog.loginview
  (:require [re-frame.core :refer [dispatch]]
            blog.state.login-handlers
            [reagent.core :as r]
            [blog.util :refer [value-of]]))

(defn loginview [user]
  (let [login-state (r/atom {:username ""
                             :password ""})]
    (fn [user]
      (if-not user
        [:div#loginview
         [:div.meta "Show the credentials"]
         [:label "Username: " [:input {:value (:username @login-state)
                                       :on-change #(swap! login-state assoc :username (value-of %))
                                       :type :text}]]
         [:label "Password: " [:input {:value (:password @login-state)
                                       :on-change #(swap! login-state assoc :password (value-of %))
                                       :type :password}]]
         [:button {:on-click #(dispatch [:log-in @login-state])} "Log in!"]]
        (let [{:keys [nickname img_location]} user]
          [:div#loginview
           [:img.user_avatar {:src img_location
                  :alt (str nickname "'s avatar")}]
           nickname " logged in"
           [:button {:on-click #(dispatch [:log-out])}"Log out!"]])))))
