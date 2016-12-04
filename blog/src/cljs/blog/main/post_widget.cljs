(ns blog.main.post-widget
  (:require [blog.settings :refer [settings]]
            [blog.state.post-subs]
            [re-frame.core :refer [subscribe dispatch]]

            [blog.util :refer [in?]]
            
            blog.state.login-subs
            blog.state.editor-handlers))

(defn comment-widget [can-edit? can-delete? {:keys [creator content created_at id]}]
  (let [{:keys [nickname img_location]} creator]
    [:div.post
     [:div.meta-actions
      (if can-edit?
        [:button "edit"])
      (if can-delete?
        [:button {:on-click #(if (js/confirm (str "Are you sure you want to delete the comment?"))
                               (dispatch [:delete-comment id]))} "delete"])]
     [:p.meta [:img.user_avatar
               {:src img_location}] "By " nickname]
     [:p.meta "Written at " (pr-str created_at)] ;; TODO add user-configurable formatting
     [:article.content content]]))

(defn post-widget [{:keys [id title content creator created_at tags amount-of-comments comments]} can-edit? can-delete?]
  (let [{:keys [nickname img_location]} creator]
    [:div.post
     [:h2 [:a.blog-title {:href (str "/blog/post/" id)} title]]
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
     [:p amount-of-comments " comments"]
     (if comments
       (into [:div
              [:h5 "Comments: "]]
             (map (partial comment-widget can-edit? can-delete?) comments)))]))

;; this sets up the state
(defn default-post-widget []
  (let [page (subscribe [:current-page])
        user (subscribe [:current-user])]
    (fn []
      (let [{:keys [posts]} @page]
        (into [:div
               [:div#title-actions (if (in? (:permissions @user) "create-post")
                                     [:a {:href "/blog/create-post"}
                                      "Create post!"])]]
              (mapv post-widget
                    posts
                    (repeat (in? (:permissions @user) "edit-post"))
                    (repeat (in? (:permissions @user) "delete-post"))))))))
