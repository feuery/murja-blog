(ns murja.api.users
  (:require [murja.db.users :as db]))

(defn is-empty [db]
  (db/everything-is-empty? db))

(defn save-user [db user]
  (db/update-user! db user))
