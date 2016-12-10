(ns blog.ragtime
  (:require [ragtime.jdbc :as jdbc]
            [ragtime.repl :as rag]
            [clojure.pprint :refer :all]
            [blog.system.current :refer :all]))

;; TODO Load these from a configuration
(defn start-db [passu]
  (let [db {:user "blogiadmin"
            :password passu
            :host "localhost"
            :port "5432"
            :db "blogdb"
            :dbname "blogdb"
            :classname "org.postgresql.Driver"
            :dbtype "postgresql"
            }]
    {:db
     {:datastore (jdbc/sql-database db)
      :migrations (jdbc/load-resources "migrations")}
     :db-spec db}))

(defn migrate
  ([{:keys [db]}]
   (if db
     (rag/migrate db)
     (throw (Exception. "db-spec nil"))))
  ([]
   (if @current-system
     (migrate (get-in @current-system [:db]))
     (throw (Exception. "System not running")))))

(defn rollback
  ([{:keys [db]}]
   (if db
     (rag/rollback db)
     (throw (Exception. "db is nil"))))
  ([]
   (if @current-system
     (rollback (get-in @current-system [:db]))
     (throw (Exception. "System not running")))))
