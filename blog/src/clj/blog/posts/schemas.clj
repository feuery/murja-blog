(ns blog.posts.schemas
  (:require [schema.core :as s]
            [blog.schema.macros :refer [allow-empty]])
  (:import [java.util Date]))

(s/defschema User
  {:username s/Str
   :nickname s/Str
   :img_location s/Str})

(s/defschema Post
   {:title s/Str     ;; title
    :id s/Num        ;; autoincrement
    :content s/Str   ;; contents
    :creator User    ;; user-id 1
    :created_at Date ;; published-date
    :tags [s/Str]    ;; categories
    :amount-of-comments s/Num ;; 0
    })

(s/defschema Imported-Post
  {:Title s/Str
   :Content s/Str
   :creator_id s/Num
   :created_at Date
   :tags [s/Str]})

(s/defschema New-post (dissoc Post :creator :created_at :amount-of-comments :id))

(s/defschema Comment
  {:content s/Str
   :creator User
   ;; This is going to be fun once these schemas are needed client side...
   :created_at Date
   :id s/Num
   (s/optional-key :children) [(s/recursive #'Comment)]})

(s/defschema New-comment
  {:content s/Str
   :parent-post-id s/Num
   :parent-comment-id (s/maybe s/Num)})
  

(s/defschema Commented-Post
  (s/conditional empty? {}
                 :else
                 (assoc Post
                        :comments [Comment]
                        :next-post-id (s/maybe s/Num)
                        :prev-post-id (s/maybe s/Num))))
