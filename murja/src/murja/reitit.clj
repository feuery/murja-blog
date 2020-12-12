(ns murja.reitit
  (:require [reitit.ring :as ring]
            [reitit.ring.middleware.muuntaja :as middleware.muuntaja]
            [reitit.ring.middleware.parameters :as middleware.params]
            [reitit.swagger-ui]
            [reitit.swagger]
            [mount.core :as mount :refer [defstate]]
            [muuntaja.core :as muuntaja]
            [reitit.coercion.spec]
            [reitit.ring.coercion :as ring.coercion]
            [org.httpkit.server :as http-kit]
            [clojure.tools.namespace.repl :as tn]

            [murja.api :as api]))

;; tags: posts, login, users, settings

(def app-routes [["" {:middleware [middleware.params/parameters-middleware]}
                  ["/api"
                   ["/posts" {:swagger {:tags ["posts"]}}]
                   ["/login" {:swagger {:tags ["login"]}}]
                   ["/users" {:swagger {:tags ["users"]}}]
                   ["/settings" {:swagger {:tags ["settings"]}}
                    ["/client-settings" {:get {:handler #'api/get-client-settings}}]]]
                  ["" {:no-doc true}
                   ["/swagger.json" {:get (reitit.swagger/create-swagger-handler)}]
                   ["/swagger/*" {:get (reitit.swagger-ui/create-swagger-ui-handler)}]]]])

(defn router [options]
  (ring/router
   app-routes
   {:data {:coercion reitit.coercion.spec/coercion
           :muuntaja muuntaja/instance
           :middleware [middleware.muuntaja/format-middleware
                        ring.coercion/coerce-exceptions-middleware
                        ring.coercion/coerce-request-middleware
                        ring.coercion/coerce-response-middleware]}}))
                        

(defn app [options]
  (ring/ring-handler (router options)
                     (ring/create-default-handler)))

;; (def server (http-kit/run-server (app {})
;;                                  {:port 3000}))

;; (http-kit/server-stop! server)

;; (http-kit/server-status server)

(defstate http-server :start (http-kit/run-server (app {})
                                                  {:port 3000
                                                   :legacy-return-value? false})
  :stop (http-kit/server-stop! http-server))

(defn go []
  (mount/start))

(defn stop []
  (mount/stop))

(defn reset []
  (stop)
  (tn/refresh :after 'dev/go))
