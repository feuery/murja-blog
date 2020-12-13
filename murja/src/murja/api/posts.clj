(ns murja.api.posts
  (:require [murja.db.posts :as db.posts]))

(defn get-titles-by-year [db & {:keys [show-hidden?]}]
  {:pre [(boolean? show-hidden?)]}
  (db.posts/get-titles-by-year db :show-hidden? show-hidden?))
