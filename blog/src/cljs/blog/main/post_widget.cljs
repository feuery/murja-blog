(ns blog.main.post-widget
  (:require [blog.settings :refer [settings]]
            [blog.state.post-subs]
            [re-frame.core :refer [subscribe dispatch]]

            [blog.util :refer [in?]]
            [blog.loginview :refer [loginview]]
            blog.state.login-subs
            blog.state.editor-handlers))

(defn post-widget [{:keys [id title content creator created_at tags amount-of-comments]} can-edit? can-delete?]
  (let [{:keys [nickname img_location]} creator]
    [:div.post
     [:h2 title]
     [:div.meta-actions
      (if can-edit?
        [:button "edit"])
      (if can-delete?
        [:button {:on-click #(if (js/confirm (str "Are you sure you want to delete post titled " title "?"))
                               (dispatch [:delete-post id]))} "delete"])]
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
               (mapv post-widget
                     posts
                     (repeat (in? (:permissions @user) "edit-post"))
                     (repeat (in? (:permissions @user) "delete-post"))))
         [:div#sidebar
          [loginview @user]]]))))
