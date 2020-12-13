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

(defn get-post-by-id [db id]
  (db.posts/get-by-id db id))
