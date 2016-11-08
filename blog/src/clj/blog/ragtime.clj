(ns blog.ragtime
  (:require [ragtime.jdbc :as jdbc]
            [ragtime.repl :as rag]))

(def config {:datastore (jdbc/sql-database {:user "blogiadmin"
                                            :password ""
                                            :host "localhost"
                                            :port "5432"
                                            :db "blogdb"
                                            :dbname "blogdb"
                                            :classname "org.postgresql.Driver"
                                            :dbtype "postgresql"
                                            })
             :migrations (jdbc/load-resources "migrations")})


(defn migrate []
  (rag/migrate config))

(defn rollback []
  (rag/rollback config))
