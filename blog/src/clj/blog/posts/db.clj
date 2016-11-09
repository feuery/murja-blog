(ns blog.posts.db
  (:require [clojure.java.jdbc :as j]
            [schema.core :as s]
            [blog.posts.schemas :refer [Post]]
            [blog.ragtime :refer [db]]
            [clojure.pprint :refer :all]
            [cheshire.core :as ch])
  (:import [org.postgresql.util PGobject]))

(comment "INSERT INTO blog.Users(Username, Password, Nickname, Img_location) VALUES ('feuer', '', 'Feuer', '');
blogdb=# INSERT INTO blog.Post (Title, Content, creator_id, tags) VALUES ('Hello World!', 'Minä olen maailman ensimmäinen teksti', 1, '["eka teksti"]'::jsonb);")

(extend-protocol j/IResultSetReadColumn
  PGobject
  (result-set-read-column [pgobj metadata idx]
    (let [type  (.getType pgobj)
          value (.getValue pgobj)]
      (case type
        "jsonb" (cond-> (ch/parse-string value keyword)
                  seq? vec)
        :else (do
                (println "Type " type " not recognized")
                value)))))
         

(s/defn get-by-id :- Post
  [id]
  (let [{:keys [username nickname img_location] :as db-row}  (j/query db ["SELECT p.Title, p.Content, p.tags, u.Username, u.Nickname, u.Img_location
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
WHERE p.ID = ?" id] :result-set-fn first)]
    (-> db-row
        (assoc :creator {:username username
                         :nickname nickname
                         :img_location img_location})
        (dissoc :username :nickname :img_location))))
