(ns blog.posts.schemas
  (:require [schema.core :as s])
  (:import [java.util Date]))

(s/defschema User
  {:username s/Str
   :nickname s/Str
   :img_location s/Str})

(s/defschema Post
  {:title s/Str
   :id s/Num
   :content s/Str
   :creator User
   :created_at Date
   :tags [s/Str]
   :amount-of-comments s/Num})

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
  (assoc Post :comments [Comment]))
