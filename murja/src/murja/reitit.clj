(ns murja.reitit
  (:require [reitit.ring :as ring]
            [reitit.ring.middleware.muuntaja :as reitit.middleware.muuntaja]
            [reitit.ring.middleware.parameters :as reitit.middleware.params]
            [reitit.swagger-ui]
            [reitit.swagger]
            [mount.core :as mount :refer [defstate]]
            [muuntaja.core :as muuntaja]
            [reitit.coercion.spec]
            [reitit.ring.coercion :as ring.coercion]
            [clojure.tools.namespace.repl :as tn]
            [cognitect.transit :as transit]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.session :as session]
            [clojure.spec.alpha :as spec]
            [murja.api :as api]

            [murja.middleware :as middleware]
            [murja.config :as config]
            [murja.db :refer [migrate rollback]]
            [murja.db.users]
            [murja.specs.updated-user :as updated-user]
            [murja.specs.login :as login]
            [murja.specs.timed-title :as timed-title]
            [murja.specs.post :as post])
  (:gen-class))

;; tags: posts, login, users, settings

(def app-routes [["/api" {:middleware [middleware/wrap-db]}
                  ["/login" {:swagger {:tags ["login"]}}
                   ["/login" {:post {:handler #'api/post-login
                                     :parameters {:body ::login/login}}}]
                   ["/logout" {:post {:handler #'api/post-logout}}]
                   ["/session" {:middleware [middleware/wrap-user]
                                :get {:handler #'api/get-session}}]]
                  ["/users" {:swagger {:tags ["users"]}}
                   ["/is-empty" {:get {:handler #'api/get-users-is-empty}}]
                   ["/save" {:middleware [middleware/wrap-user
                                          [middleware/can? "edit-self"]]
                             :post {:handler #'api/post-users-save
                                    :parameters {:body ::updated-user/updated-user}}}]]
                  ["/settings" {:swagger {:tags ["settings"]}}
                   ["/client-settings" {:get {:handler #'api/get-client-settings}}]]

                  ["/posts" {:swagger {:tags ["posts"]}}
                   ["/titles" {:get {:handler #'api/get-posts-titles
                                     :summary "Returns titles, tags, months and years for the title-widget"
                                     :responses {200 {:body (spec/* ::timed-title/Timed-Title)}}}}]
                   ;; TODO this WILL break if used in an environment with more than one writer
                   ["/all-titles" {:middleware [middleware/wrap-user
                                                [middleware/can? "edit-post"]]
                                  :get {:handler #'api/get-posts-all-titles
                                         :summary "Same as /titles, but auths that requester has edit-post - permission"
                                         :responses {200 {:body (spec/* ::timed-title/Timed-Title)}}}}]

                   ["/existing-landing-page" {:get {:summary "Returns either an empty string or the title of already existing landing page"
                                                    :handler #'api/get-existing-landing-page}}]
                   ["/post" {:post {:summary "Writes a new post into the db"
                                    :middleware [middleware/wrap-user
                                                 [middleware/can? "create-post"]]
                                    :parameters {:body ::post/New-post}
                                    :handler #'api/create-post} ;; create-post
                             :put {:summary "Edits a post"
                                   :parameters {:body ::post/Edited-post}
                                   :middleware [middleware/wrap-user
                                                [middleware/can? "edit-post"]]
                                   :handler #'api/edit-post}}]
                   ["/post"
                    ["/:id" {:parameters {:path {:id int?}}
                             :get {:summary "Returns a post per its id"
                                   :parameters {:path {:id int?}}
                                   :handler #'api/get-post-id}
                             :delete {:summary "Deletes a post and returns its id"
                                      :middleware [middleware/wrap-user
                                                   [middleware/can? "delete-post"]]
                                      :handler #'api/delete-post-id}}]
                    ["/:id" {:parameters {:path {:id int?}}}
                     ["/comment" {:post {:summary "Comments a post and returns it with the new comment appended"
                                         :parameters {:body ::post/New-comment}
                                         :middleware [middleware/wrap-user
                                                      [middleware/can? "comment-post"]]
                                         :handler #'api/comment-post}}]

                     ["/versions" {:get {:handler #'api/get-id-versions}}]
                     ["/version/:version" {:parameters {:path {:id int?
                                                                 :version int?}}

                                             :get {:summary "Returns an old version of the post and the current comments"
                                                   :handler #'api/get-post-version}

                                             :delete {:summary "Deletes a version of a post"
                                                      :middleware [middleware/wrap-user
                                                                   [middleware/can? "delete-post"]]
                                                      :handler #'api/delete-post-version}}]
                     ["/allow-hidden/:allow-hidden" {:middleware [middleware/wrap-user
                                                                    [middleware/can? "edit-post"]]
                                                       :summary "Returns a post per its id. Can return also hidden posts if edit-post permission is held"
                                                       :get {:parameters {:path {:id int? :allow-hidden boolean?}}
                                                             :handler #'api/get-post-id-allow-hidden}}]]]
                   
                   ["/comment/:id" {:delete {:summary "Deletes a comment and returns its id"
                                             :middleware [middleware/wrap-user
                                                          [middleware/can? "delete-comment"]]
                                             :parameters {:path {:id int?}}
                                             :handler #'api/delete-comment-id}
                                    :conflicting true}]

                   ["/all/:limit" {:get {:summary "Returns first $limit of all posts sorted by their creation date DESC"
                                         :parameters {:path {:limit int?}}
                                         :handler #'api/get-all-limit}
                                   :conflicting true}]
                   ["/page/:page/page-size/:page-size" {:get {:summary "Returns a page of specific size. Posts are sorted by their creation date DESC"
                                                              :parameters {:path {:page int?
                                                                                  :page-size int?}}
                                                              :handler #'api/get-page}}]
                    
                   ]]
                 ["" {:no-doc true}
                  ["/site.css" {:get {:handler #'api/get-site.css}}]
                  ["/swagger.json" {:get (reitit.swagger/create-swagger-handler)}]
                  ["/swagger/*" {:get (reitit.swagger-ui/create-swagger-ui-handler)}]
                  ["/blog/*" {:middleware [middleware/wrap-db]
                              :get {:handler #'api/get-frontend}}]
                  ["/" {:get {:handler (fn [_]
                                         {:status  302
                                          :headers {"Location" "/blog/"}
                                          :body    ""})}}]]])

(defn router [options]
  (ring/router
   app-routes
   {:data {:coercion reitit.coercion.spec/coercion
           :muuntaja (-> muuntaja/default-options
                         muuntaja/create)
           :middleware [reitit.middleware.params/parameters-middleware
                        reitit.middleware.muuntaja/format-middleware
                        ring.coercion/coerce-exceptions-middleware
                        ring.coercion/coerce-request-middleware
                        ring.coercion/coerce-response-middleware]}}))
                        

(defn app [options]
  (ring/ring-handler (router options)
                     (ring/create-default-handler)
                     {:middleware [session/wrap-session]}))

(defstate http-server
  :start (run-jetty (app {}) {:port (get-in config/config [:http :port])
                              :join? false
                              :host (get-in config/config [:http :host])})
  :stop (.stop http-server))

(defn go []
  (mount/start))

(defn stop []
  (mount/stop))

(defn reset []
  (stop)
  (go))

(defn -main [& _]
  (go)
  (println "Murja running!")
  (migrate)
  (println "Migrated!"))
