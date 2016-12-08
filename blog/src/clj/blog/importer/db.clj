(ns blog.importer.db
  (:require [clojure.java.jdbc :as j]
            [clojure.pprint :refer :all]
            [clojure.xml :as xml]
            [clojure.string :as str]
            [schema.core :as s]
            [blog.posts.schemas :as sc]
            [blog.util :refer :all]
            [cheshire.core :as ch]
            [feedparser-clj.core :refer :all]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [clj-time.coerce :refer [from-date]])
  (:import [java.io ByteArrayInputStream]
           [org.postgresql.util PGobject]))

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
     ;; Google puts a lot of rubbish in their atom "backup" feeds
     (filter (fn [{:keys [uri categories] :as aaaa}]
               ;; #dbg
               (let [category (first categories)]
                 (and (not (.contains (:name category) "comment"))
                      (.contains uri "post"))))
             (:entries data)))))

(defn import-atom! [{:keys [db-spec] :as sys} atom-xml]
  (println "Starting import!")
  (let [blogdata (parse-atom! atom-xml (get-importer-user-id! sys ))]
    (println "Found " (count blogdata) " posts")
    (j/insert-multi! db-spec :blog.Post
                     blogdata)))
