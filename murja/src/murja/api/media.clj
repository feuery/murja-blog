(ns murja.api.media
  (:require [clojure.java.io :as io]
            [hugsql.core :refer [def-db-fns]] )
  (:import [org.apache.commons.io IOUtils]))

(def-db-fns "media.sql")

(defn save-image [{:keys [db-spec]} {:keys [filename tempfile] :as file}]
  (let [bytedata (IOUtils/toByteArray (io/input-stream tempfile))]
    (first (insert-media db-spec {:name filename
                                  :data bytedata}))))

(defn get-picture [{:keys [db-spec]} id]
  (get-media db-spec {:id id}))

(defn list-pictures [{:keys [db-spec]}]
  (list-pictures* db-spec))

(defn delete-pictures [{:keys [db-spec]} ids]
  (doseq [id ids]
    (delete-picture* db-spec {:id id})))

(defn posts-referencing [{:keys [db-spec]} id]
  (select-referencing-posts* db-spec {:id id}))
