(ns murja.api.users
  (:require [murja.db.users :as db]))

(defn is-empty [db]
  (db/everything-is-empty? db))
