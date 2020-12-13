(ns murja.api.posts
  (:require [murja.db.posts :as db.posts]))

(defn get-titles-by-year [db]
  (db.posts/get-titles-by-year db))
