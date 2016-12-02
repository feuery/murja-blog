(ns blog.main.post-widget
  (:require [blog.settings :refer [settings]]
            [blog.state.post-subs]
            [re-frame.core :refer [subscribe dispatch]]

            [blog.util :refer [in?]]
            [blog.loginview :refer [loginview]]
            blog.state.login-subs
            blog.state.editor-handlers))

(defn post-widget [{:keys [title content creator created_at tags amount-of-comments]}]
  (let [{:keys [nickname img_location]} creator]
    [:div.post
     [:h2 title]
     [:p.meta [:img.user_avatar
               {:src img_location}] "By " nickname]
     [:p.meta "Written at " (pr-str created_at)] ;; TODO add user-configurable formatting
     [:article.content content]
     ;; TODO make a link to the post-and-its-comments view
     [:p amount-of-comments " comments"]]))

;; this sets up the state
(defn default-post-widget []
  (let [page (subscribe [:current-page])
        user (subscribe [:current-user])]
    (fn []
      (let [{:keys [posts]} @page]
        ;; I hate css's layout system
        [:div#container
         (into [:div#page
                [:div#title-actions (if (in? (:permissions @user) "create-post")
                                      [:button {:on-click #(dispatch [:start-new-post])}
                                       "Create post!"])]]
               (mapv post-widget posts))
         [:div#sidebar
          [loginview @user]]]))))
