(ns murja.reitit
  (:require [reitit.ring :as ring]
            [reitit.ring.middleware.muuntaja :as middleware.muuntaja]
            [muuntaja.core :as muuntaja]
            [reitit.coercion.spec]
            [org.httpkit.server :as http-kit]))

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
