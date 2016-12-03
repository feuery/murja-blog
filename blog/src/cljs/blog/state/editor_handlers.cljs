(ns blog.state.editor-handlers
  (:require [re-frame.core :refer [dispatch reg-event-db reg-event-fx trim-v]]
            [clojure.string :as str]))

(reg-event-db
 :start-new-post
 (fn [db _]
   (assoc db :editor-visible? true
          :edited-post {:status :new
                        :title ""
                        :content ""
                        ;; tags will be split to a vector on send
                        :tags ""})))

(reg-event-db
 :set-editor-visibility
 [trim-v]
 (fn [db [visible?]]
   (assoc db :editor-visible? visible?)))

(reg-event-db
 :set-post-content
 [trim-v]
 (fn [db [content]]
   (assoc-in db [:edited-post :content] content)))

(reg-event-db
 :set-post-title
 [trim-v]
 (fn [db [title]]
   (assoc-in db [:edited-post :title] title)))

(reg-event-db
 :set-post-tags
 [trim-v]
 (fn [db [tags]]
   (assoc-in db [:edited-post :tags] tags)))

(reg-event-fx
 :publish-post
 (fn [{:keys [db]} _]
   (let [{:keys [edited-post]} (-> db
                                   (update-in [:edited-post :tags]
                                              (fn [tags]
                                                (str/split tags #",\s*")))
                                   (update :edited-post dissoc :status))]
     (println (pr-str edited-post))
     {:post {:url "/api/posts/post/post"
             :body edited-post
             :dispatch-key :post-published}
      :db db})))

(reg-event-fx
 :post-published
 [trim-v]
 (fn [{:keys [db]} [new-post]]
   {:alert "Post published!"
    :db (-> db
            (assoc :editor-visible? false)
            (dissoc :edited-post )
            (update-in [:page :posts] (fn [posts]
                                        (vec (concat [new-post] posts)))))}))
