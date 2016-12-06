(ns blog.importer.db
  (:require [clojure.java.jdbc :as j]
            [schema.core :as s]
            [blog.posts.schemas :as sc]
            [blog.util :refer :all]
            [clojure.pprint :refer :all]
            [cheshire.core :as ch]
            [clojure.xml :as xml]
            [feedparser-clj.core :refer :all]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [clj-time.coerce :refer [from-date]])
  (:import [java.io ByteArrayInputStream]
           [org.postgresql.util PGobject]))

(tf/unparse (tf/formatter "yyyy-MM-dd") (t/now))

(extend-protocol j/ISQLValue
  java.util.Date
  (sql-value [val]
    (doto (PGobject.)
      (.setType "timestamp")
      (.setValue (tf/unparse (tf/formatter "yyyy-MM-dd") (from-date val))))))

(defn get-importer-user-id! [{:keys [db-spec]}]
  (j/query db-spec ["SELECT * FROM blog.Users ORDER BY ID ASC LIMIT 1"]
           :result-set-fn first
           :row-fn :id))

(s/defn ^:always-validate
  parse-atom! :- [sc/Imported-Post]
  [atom-xml :- s/Str
   importer-user]
  
  (let [data (parse-feed (ByteArrayInputStream. (.getBytes atom-xml)))]
    (mapv
     (fn [{:keys [categories contents title published-date]}]
       {:Title title
        :created_at published-date
        :tags (mapv :name categories)
        :creator_id importer-user
        :Content (->> contents
                      (map :value)
                      (reduce str))})
     (:entries data))))

(defn import-atom! [{:keys [db-spec] :as sys} atom-xml]
  (let [blogdata (parse-atom! atom-xml (get-importer-user-id! sys ))]
    ;; (doseq [{:keys [title created_at tags creator-id content]} blogdata]
    (j/insert-multi! db-spec :blog.Post
                     blogdata)))

;; (def xmldoc
;;   (memoize (fn [] (xml->doc -data))))

;; (-> ($x "/feed/entry" (xmldoc))
;;     first)

