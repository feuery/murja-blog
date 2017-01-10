(ns blog.system.server
  (:require [blog.server :refer :all]))

(defn start-server! [port]
  (run-server! port))

(defn stop-server-sys! [{:keys [server] :as sys}]
  (if server
    (do
      (println "Stopping " server)
      (stop-server! server))
    (println "No server, sys: " sys))
  (dissoc sys :server))
