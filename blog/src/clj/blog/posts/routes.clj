(ns blog.posts.routes
  (:require [schema.core :as s]
            [compojure.api.core :as c :refer [GET POST PUT DELETE]] 
            [compojure.api.sweet :as sw :refer [context]]
            [ring.util.http-response :refer [internal-server-error ok]]
            [clojure.pprint :refer :all]
            [blog.posts.schemas :as post-sc]
            [blog.posts.db :as pdb]
            [blog.util :refer [destructure-db]]))

(def routes
  (context "/posts" []
           :sys sys
           :tags ["posts"]
           (GET "/:id" []
                :path-params [id :- s/Int]
                :return post-sc/Commented-Post
                :summary "Returns a post per its id"
                (destructure-db [sys]
                                (ok (pdb/get-by-id db id))))
           (GET "/all/:limit" []
                :return [post-sc/Post]
                :path-params [limit :- s/Int]
                :summary "Returns first $limit of all posts sorted by their creation date DESC"
                (destructure-db [sys]
                                (pdb/get-all db limit)))
           (GET "/" []
                :return [post-sc/Post]
                :summary "Returns all posts sorted by their creation date DESC"
                (destructure-db [sys]
                                (pdb/get-all db nil)))))
