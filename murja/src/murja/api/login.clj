(ns murja.api.login
  (:require [murja.db.login :as db]))

(defn do-login [db {:keys [username password]}]
  (db/authenticate? db username password))

(defn user-groups [db {:keys [username]}]
  (db/user-groups db username))
