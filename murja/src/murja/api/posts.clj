(ns murja.api.posts
  (:require [murja.db.posts :as db.posts]))

(defn get-titles-by-year [db & {:keys [show-hidden?]}]
  {:pre [(boolean? show-hidden?)]}
  (db.posts/get-titles-by-year db :show-hidden? show-hidden?))

(defn get-existing-landing-page [db]
  (db.posts/get-landing-page-title db))

(defn get-post-version-by-id [db id version]
  (assert (some? id))
  (assert (some? version))
  (db.posts/get-versioned-by-id db id version))

(defn get-post-by-id [db id & {:keys [show-hidden?]}]
  (db.posts/get-by-id db id :allow-hidden? show-hidden?))


(defn get-post-versions [db post-id]
  (db.posts/post-versions db post-id))

(defn delete-post-version [db post-id version-id]
  (db.posts/delete-by-id db post-id version-id)
  {:deleted-id post-id
   :deleted-version version-id})
