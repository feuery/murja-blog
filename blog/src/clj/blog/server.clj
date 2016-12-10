(ns blog.server
  (:require [blog.handler :refer [app]]
            [config.core :refer [env]]
            [ring.adapter.jetty :refer [run-jetty]]))

(defn run-server! [port]
  {:pre [(number? port)]}
  (run-jetty app {:port port :join? false}))

(defn stop-server! [s]
  (if s
    (.stop s)))
