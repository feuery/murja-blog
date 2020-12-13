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

            [murja.api :as api]
            [murja.middleware :as middleware]
            [murja.db :refer [migrate rollback]]
            [murja.db.users]
            [murja.specs.updated-user :as updated-user]
            [murja.specs.login :as login]))

;; tags: posts, login, users, settings

(def app-routes [["/api" {:middleware [middleware/wrap-db]}
                  ["/posts" {:swagger {:tags ["posts"]}}]
                  ["/login" {:swagger {:tags ["login"]}}
                   ["/login" {:post {:handler #'api/post-login
                                     :parameters {:body ::login/login}}}]
                   #_["/authed" {:middleware [middleware/wrap-user
                                            [middleware/can? "create-comment"]]
                               :get {:handler (fn [_]
                                      {:status 200
                                       :body "You're in!"})}}]]
                  ["/users" {:swagger {:tags ["users"]}}
                   ["/is-empty" {:get {:handler #'api/get-users-is-empty}}]
                   ["/save" {:middleware [middleware/wrap-user
                                          [middleware/can? "edit-self"]]
                             :post {:handler #'api/post-users-save
                                    :parameters {:body ::updated-user/updated-user}}}]]
                  ["/settings" {:swagger {:tags ["settings"]}}
                   ["/client-settings" {:get {:handler #'api/get-client-settings}}]]
                  ["/log-session" {:get {:handler (fn [{:keys [session]}]
                                                    (clojure.pprint/pprint session)
                                                    {:status 204})}}]]
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



