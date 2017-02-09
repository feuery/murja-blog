(ns blog.posts.routes
  (:require [schema.core :as s]
            [compojure.api.core :as c :refer [GET POST PUT DELETE]] 
            [compojure.api.sweet :as sw :refer [context]]
            [ring.util.http-response :refer [internal-server-error ok]]
            [clojure.pprint :refer :all]
            [blog.posts.schemas :as post-sc]
            [blog.posts.db :as pdb]
            [blog.util :refer [destructure-db]]
            [blog.access :refer [can?]]
            [blog.date-schemas :refer [Timed-Title]]))

(def routes
  (context "/posts" []
           :sys sys
           :tags ["posts"]

           (GET "/titles" []
                :return [Timed-Title]
                :summary "Returns titles, tags, months and years for the title-widget"
                (destructure-db [sys]
                                (ok
                                 (pdb/get-titles-by-year db))))
           (GET "/all-titles" []
                :return [Timed-Title]
                :summary "Same as /titles, but auths that requester has edit-post - permission"
                :current-user user
                :auth-rules (partial can? "edit-post")
                (destructure-db [sys]
                                (ok (pdb/get-titles-by-year db :show-hidden? true))))
                
           (GET "/existing-landing-page" []
                :return post-sc/Landing-page-result
                :summary "Returns either an empty string or the title of already existing landing page"
                (destructure-db [sys]
                                (ok (pdb/get-landing-page-title db))))

           (GET "/:id" []
                :path-params [id :- s/Int]
                :return post-sc/Commented-Post
                :summary "Returns a post per its id"
                (destructure-db [sys]
                                (ok (pdb/get-by-id db id))))

           (GET "/:id/allow-hidden/:allow-hidden" []
                :path-params [id :- s/Int
                              allow-hidden :- s/Bool]
                :return post-sc/Commented-Post
                :summary "Returns a post per its id. Can return also hidden posts if edit-post permission is held"
                :current-user user
                :auth-rules (partial can? "edit-post")
                (destructure-db [sys]
                                (ok (pdb/get-by-id db id :allow-hidden? allow-hidden))))
           
           (DELETE "/:id" []
                   :path-params [id :- s/Int]
                   :return s/Int
                   :summary "Deletes a post and returns its id"
                   :auth-rules (partial can? "delete-post")
                   (destructure-db [sys]
                                   (pdb/delete-by-id db id)
                                   (ok id)))
           (DELETE "/comments/:id" []
                   :path-params [id :- s/Int]
                   :return s/Int
                   :summary "Deletes a post and returns its id"
                   :auth-rules (partial can? "delete-comment")
                   (destructure-db [sys]
                                   (pdb/delete-comment-by-id db id)
                                   (ok id)))
           
           (GET "/all/:limit" []
                :return [post-sc/Post]
                :path-params [limit :- s/Int]
                :summary "Returns first $limit of all posts sorted by their creation date DESC"
                (destructure-db [sys]
                                (pdb/get-all db limit)))
           (GET "/page/:page/page-size/:page-size" []
                :return {:last-page? s/Bool
                         :posts [post-sc/Post]}
                :path-params [page :- s/Int
                              page-size :- s/Int]
                :summary "Returns a page of specific size. Posts are sorted by their creation date DESC"
                (destructure-db [sys]
                                (ok
                                 {:posts (pdb/get-page db page page-size)
                                  :last-page? (zero? (count (pdb/get-page db (inc page) page-size)))})))
           (GET "/" []
                :return [post-sc/Post]
                :summary "Returns all posts sorted by their creation date DESC"
                (destructure-db [sys]
                                (pdb/get-all db nil)))

           (context "/post" []
                    :current-user user
                    (POST "/post" []
                          :body [post post-sc/New-post]
                          :auth-rules (partial can? "create-post")
                          :return post-sc/Post
                          :summary "Writes a new post into the db"                          
                          (destructure-db [sys]
                                          (pdb/save-post! db user post)
                                          (ok (first (pdb/get-page db 1 1)))))
                    (POST "/edit" []
                          :body [post post-sc/edited-post]
                          :auth-rules (partial can? "edit-post")
                          :summary "Edits a post"
                          :return {:success? s/Bool}
                          (destructure-db [sys]
                                          (pdb/edit-post! db user post)
                                          (ok {:success? true})))
                          
                    (POST "/comment" []
                          :body [comment post-sc/New-comment]
                          :summary "Comments a post and returns it with the new comment appended"
                          :return post-sc/Commented-Post
                          :auth-rules (partial can? "comment-post")
                          (destructure-db [sys]
                                          (let [{post-id :parent-post-id} comment]
                                            (pdb/comment-post! db user comment)
                                            (ok (pdb/get-by-id db post-id))))))))
                          
                 
  
