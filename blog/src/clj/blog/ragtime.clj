(ns blog.ragtime
  (:require [ragtime.jdbc :as jdbc]
            [ragtime.repl :as rag]))

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

(defn migrate [{:keys [db-spec]}]
  (rag/migrate db-spec))

(defn rollback [{:keys [db-spec]}]
  (rag/rollback db-spec))
