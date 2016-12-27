(ns blog.main.single-post
  (:require [blog.state.post-subs]
            [re-frame.core :refer [subscribe dispatch]]
            [reagent.core :as r]

            [blog.util :refer [value-of in?]]
            [blog.main.post-widget :refer [post-widget]]))

(defn comment-box [post-id post-title can-comment?]
  (let [comment (r/atom "")]
    (fn [post-id post-title can-comment?]
      [:div
       (if can-comment?
         [:div.commenting-area
          [:label "Commenting on " post-title]
          [:textarea {:on-change #(reset! comment (value-of %))
                      :value @comment}]
          [:button {:on-click #(do
                                 (dispatch [:comment-on post-id @comment])
                                 (reset! comment ""))}
           "Comment!"]]
         [:div "Commenting disabled"])])))

(defn single-post-widget []
  (let [selected-post (subscribe [:selected-post])
        user (subscribe [:current-user])]
    (fn []
      (if @selected-post
        [:div
         [post-widget @selected-post
          (in? (:permissions @user) "edit-post")
          (in? (:permissions @user) "delete-post")]
          
         [comment-box (:id @selected-post)
                      (:title @selected-post)
                      (in? (:permissions @user) "comment-post")]]
        [:div "No post selected"]))))
