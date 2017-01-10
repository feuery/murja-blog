(ns blog.loginview
  (:require blog.state.login-handlers
            [reagent.core :as r]
            [blog.util :refer [value-of]]
            [re-frame.core :refer [subscribe dispatch]]
            [blog.grouper :refer [def-grouper]]))

(defn user-ctrl-panel [{:keys [nickname img_location] :as user}]
  [:div#loginview
   [:p [:img.user_avatar {:src img_location
                          :alt (str nickname "'s avatar")}]
    nickname " logged in"]
   
   [:a {:href "/blog/user-editor"} "Edit your information"]
   [:a {:href "/blog/import/atom"} "Import atom content"]
   [:button {:on-click #(dispatch [:log-out])}"Log out!"]])  

(defn loginview []
  (let [default {:username ""
                 :password ""}
        login-state (r/atom default)
        user (subscribe [:current-user])
        settings (subscribe [:settings])]
    (fn []
      (if-not @user
        [:div
         [:div#loginview
          [:div.meta "Show the credentials"]
          [:label "Username: " [:input {:value (:username @login-state)
                                        :on-change #(swap! login-state assoc :username (value-of %))
                                        :type :text}]]
          [:label "Password: " [:input {:value (:password @login-state)
                                        :on-change #(swap! login-state assoc :password (value-of %))
                                        :type :password}]]
          [:button {:on-click #(dispatch [:log-in @login-state])} "Log in!"]

          [:a {:href "/blog/register"} "Register"]]
         [def-grouper]]
        (do
          (reset! login-state default)
          [user-ctrl-panel @user])))))
