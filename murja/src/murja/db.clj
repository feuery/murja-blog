(ns murja.db
  (:require [mount.core :refer [defstate]]
            [ragtime.jdbc :as jdbc]
            [ragtime.repl :as rag]
            [hugsql.core :as hug]
            [hugsql.adapter.clojure-java-jdbc :as ad]
            [murja.config :as config]))

(defstate db 
  :start (do
           (hug/set-adapter! (ad/hugsql-adapter-clojure-java-jdbc))
           {:db
            {:datastore (jdbc/sql-database (:db config/config))
             :migrations (jdbc/load-resources "migrations")}
            :db-spec (:db config/config)})
  :stop nil)


(defn migrate
  ([{:keys [db]}]
   (if (and db
            (contains? db :datastore)
            (contains? db :migrations))
     (rag/migrate db)
     (throw (Exception. "db-spec nil"))))
  ([]
   (migrate db)))

(defn rollback
  ([{:keys [db]}]
   (if db
     (rag/rollback db)
     (throw (Exception. "db is nil"))))
  ([]
   (rollback db)))


