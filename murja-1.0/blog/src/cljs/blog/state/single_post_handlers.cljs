(ns blog.state.single-post-handlers
  (:require [re-frame.core :refer [dispatch reg-event-db reg-event-fx trim-v]]))

(reg-event-fx
 :load-post-full
 [trim-v]
 (fn [{:keys [db]} [post-id]]
   {:db db
    :get {:url (str "/api/posts/" post-id)
          :dispatch-key :post-loaded}}))

(reg-event-db
 :post-loaded
 [trim-v]
 (fn [db [post]]
   (assoc db :selected-post post)))

(reg-event-fx
 :comment-on
 [trim-v]
 (fn [{:keys [db]} [post-id comment]]
   {:post {:url "/api/posts/post/comment"
           :body {:content comment
                  :parent-post-id post-id
                  ;; TODO Recursive commenting isn't yet supported in the frontend
                  :parent-comment-id nil}
           :dispatch-key :comment-published}
    :db db}))

(reg-event-fx
 :comment-published 
 [trim-v]
 (fn [{:keys [db]} [post]]
   (println "Post: " (pr-str post))
   {:alert "Comment published!"
    :db (assoc db :selected-post post)}))

(reg-event-fx
 :delete-comment
 [trim-v]
 (fn [{:keys [db]} [comment-id]]
   {:delete {:url (str "/api/posts/comments/" comment-id)
             :dispatch-key :comment-deleted}
    :db db}))

(reg-event-db
 :comment-deleted
 [trim-v]
 (fn [db [id]]
   (update-in db [:selected-post :comments] (fn [comments]
                                              (filterv #(not= (:id %) id) comments)))))
