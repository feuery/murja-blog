(ns blog.posts.schemas
  (:require [schema.core :as s]))

(s/defschema User
  {:username s/Str
   :nickname s/Str
   :img_location s/Str})

(s/defschema Post
  {:title s/Str
   :content s/Str
   :creator User
   :tags [s/Str]})
