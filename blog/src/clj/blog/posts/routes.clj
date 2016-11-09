(ns blog.posts.routes
  (:require [schema.core :as s]
            [compojure.api.core :as c :refer [GET POST PUT DELETE]] 
            [compojure.api.sweet :as sw :refer [context]]
            [ring.util.http-response :refer [ok]]
            [clojure.pprint :refer :all]
            [blog.posts.schemas :refer [Post]]
            [blog.posts.db :refer [get-by-id]]))

(def routes
  (context "/posts" []
           :tags ["posts"]
           (GET "/:id" []
                :path-params [id :- s/Int]
                :return Post
                (ok (get-by-id id)))))
