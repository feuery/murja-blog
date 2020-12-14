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
            [murja.db :refer [migrate rollback]]
            [murja.db.users]
            [murja.specs.updated-user :as updated-user]
            [murja.specs.login :as login]
            [murja.specs.timed-title :as timed-title]))

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
                   ["/titles" {:conflicting true
                               :get {:handler #'api/get-posts-titles
                                     :summary "Returns titles, tags, months and years for the title-widget"
                                     :responses {200 {:body (spec/* ::timed-title/Timed-Title)}}}}]
                   ;; TODO this WILL break if used in an environment with more than one writer
                   ["/all-titles" {:middleware [middleware/wrap-user
                                                [middleware/can? "edit-post"]]
                                   :conflicting true
                                   :get {:handler #'api/get-posts-all-titles
                                         :summary "Same as /titles, but auths that requester has edit-post - permission"
                                         :responses {200 {:body (spec/* ::timed-title/Timed-Title)}}}}]

                   ["/existing-landing-page" {:conflicting true
                                              :get {:summary "Returns either an empty string or the title of already existing landing page"
                                                    :handler #'api/get-existing-landing-page}}]
                   ["/:id/allow-hidden/:allow-hidden" {:middleware [middleware/wrap-user
                                                                    [middleware/can? "edit-post"]]
                                                       :summary "Returns a post per its id. Can return also hidden posts if edit-post permission is held"
                                                       :get {:parameters {:path {:id int? :allow-hidden boolean?}}
                                                             :handler #'api/get-post-id-allow-hidden}}]
                   ["/:id/version/:version" {:parameters {:path {:id int?
                                                                 :version int?}}

                                             :get {:summary "Returns an old version of the post and the current comments"
                                                   :handler #'api/get-post-version}

                                             :delete {:summary "Deletes a version of a post"
                                                      :middleware [middleware/wrap-user
                                                                   [middleware/can? "delete-post"]]
                                                      :handler #'api/delete-post-version}}]
                   ["/:id/versions" {:get {:parameters {:path {:id int?}}
                                           :handler #'api/get-id-versions}}]
                   ["/:id" {:get {:summary "Returns a post per its id"
                                  :parameters {:path {:id int?}}
                                  :handler #'api/get-post-id}
                            :conflicting true}]]]
                 ["" {:no-doc true}
                  ["/swagger.json" {:get (reitit.swagger/create-swagger-handler)}]
                  ["/swagger/*" {:get (reitit.swagger-ui/create-swagger-ui-handler)}]]])

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
  :start (run-jetty (app {}) {:port 3000 :join? false})
  :stop (.stop http-server))

(defn go []
  (mount/start))

(defn stop []
  (mount/stop))

(defn reset []
  (stop)
  (go))



