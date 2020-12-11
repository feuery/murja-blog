(ns murja.reitit
  (:require [reitit.ring :as ring]
            [reitit.ring.middleware.muuntaja :as middleware.muuntaja]
            [mount.core :as mount :refer [defstate]]
            [muuntaja.core :as muuntaja]
            [reitit.coercion.spec]
            [org.httpkit.server :as http-kit]
            [clojure.tools.namespace.repl :as tn]))

(defn create-muuntaja []
  (-> muuntaja/default-options
      ;; (update-in [:formats "application/transit+json"] merge
      (muuntaja/create)))

(defn get-ping [_]
  {:status 200
   :body "MOI"})

(def app-routes [["/api" 
                  ["/ping" {:get {:handler #'get-ping}}]]])

(defn router [options]
  (ring/router
   app-routes
   {:data {:coercion reitit.coercion.spec/coercion
           :muuntaja (create-muuntaja)
           #_#_:middleware [middleware.muuntaja/format-middlewre]}}))

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
