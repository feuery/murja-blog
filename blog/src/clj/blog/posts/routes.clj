(ns blog.posts.routes
  (:require [schema.core :as s]
            [compojure.api.core :as c :refer [GET POST PUT DELETE]] 
            [compojure.api.sweet :as sw :refer [context]]
            [ring.util.http-response :refer [internal-server-error ok]]
            [clojure.pprint :refer :all]
            [blog.posts.schemas :as post-sc]
            [blog.posts.db :as pdb]))

(declare db)

(defmacro if-db [& forms]
  `(if (and ~'db
            (:db-spec ~'db))
     (do
       ~@forms)
     (internal-server-error "Db is a lie")))

(def routes
  (context "/posts" []
           :sys _
           :tags ["posts"]
           (GET "/:id" rq
                :path-params [id :- s/Int]
                :return post-sc/Commented-Post
                (if-db
                 (pdb/get-by-id db id)))
           (GET "/all/:limit" rq
                :return [post-sc/Post]
                :path-params [limit :- (s/maybe s/Int)]
                (if-db
                 (pdb/get-all db limit)))))
