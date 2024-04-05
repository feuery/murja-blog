(ns user
  (:require [clojure.java.jdbc :as j]
            [murja.db.users :refer [register-user!]]
            [murja.config :refer [config]]
            [murja.security :refer [sha-512]]))

(defn prepare-playwright-environment []
  (j/execute! (:db config) ["DELETE FROM blog.Users"])
  (j/execute! (:db config) ["DELETE FROM blog.Media"])
  (register-user! {:db-spec (:db config)}
                  "playwrighte"
                  "Playwright-user"
                  ""
                  "p4ssw0rd"))

#_(prepare-playwright-environment)
