(ns blog.system
  (:require [blog.system.server :refer :all]
            [blog.ragtime :refer :all]
            [blog.system.current :refer :all])
  (:gen-class))

(defn go
  ([port db-passwd]
   (reset! current-system {:db (start-db db-passwd)
                           :server (start-server! port)}))
  ([]
   (go 3000 "")))

(defn stop []
  ;; db don't need no stopping
  (swap! current-system stop-server-sys!))

(defn reset []
  (stop)
  (go))

(defn -main [& _]
  (go)
  (println "System running!")
  (migrate)
  (println "Migrated!"))
