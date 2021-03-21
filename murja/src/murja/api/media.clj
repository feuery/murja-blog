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
