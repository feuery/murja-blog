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
                   ]]
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



