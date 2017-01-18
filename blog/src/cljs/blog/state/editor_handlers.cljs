(ns blog.state.editor-handlers
  (:require [re-frame.core :refer [dispatch reg-event-db reg-event-fx trim-v]]
            [clojure.string :as str]))

(reg-event-db
 :start-new-post
 (fn [db _]
   (assoc db
          :edited-post {:status :new
                        :title ""
                        :content ""
                        ;; tags will be split to a vector on send
                        :tags ""})))

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
     (if-not (contains? (apply hash-set (:tags edited-post)) "landing-page")
       {:post {:url "/api/posts/post/post"
               :body edited-post
               :dispatch-key :post-published}
        :db db}
       {:get {:url "/api/posts/existing-landing-page"
              :dispatch-key :publishing-landing-page-query}
        :db (assoc db :post-to-publish edited-post)}))))

(reg-event-fx
 :publishing-landing-page-query
 (fn [{:keys [db]} landing-page-result]
   (let [{:keys [title]} (second landing-page-result)
         {:keys [post-to-publish]} db
         db (dissoc db :post-to-publish)]
     (if (and (not (empty? landing-page-result))
              (js/confirm (str "There is already a landing page with title \"" title "\". Do you want to set this new post as the new landing page?")))
       {:post {:url "/api/posts/post/post"
               :body post-to-publish
               :dispatch-key :post-published}
        :db db}
       {:db db
        :alert "Post wasn't published. If you want to publish it without setting a new landing page, remove the landing-page tag"}))))
   


(reg-event-fx
 :post-published
 [trim-v]
 (fn [{:keys [db]} [new-post]]
   {:alert "Post published!"
    :redirect-to "/blog/"
    :db (-> db
            (dissoc :edited-post)
            (update-in [:page :posts] (fn [posts]
                                        (vec (concat [new-post] posts)))))}))
