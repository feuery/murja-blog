(ns blog.posts.db
  (:require [clojure.java.jdbc :as j]
            [schema.core :as s]
            [blog.posts.schemas :as sc]
            [blog.util :refer :all]
            [clojure.pprint :refer :all]
            [cheshire.core :as ch])
  (:import [org.postgresql.util PGobject]))
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
(comment "INSERT INTO blog.Users(Username, Password, Nickname, Img_location) VALUES ('feuer', '', 'Feuer', '');
blogdb=# INSERT INTO blog.Post (Title, Content, creator_id, tags) VALUES ('Hello World!', 'Minä olen maailman ensimmäinen teksti', 1, '["eka teksti"]'::jsonb);"
         "INSERT INTO blog.comment (parent_post_id, Content, creator_id) VALUES (1, 'HEIMOI', 1)"
         )

(defn add-user [row username nickname img_location]   
     (assoc row :creator {:username username
                          :nickname nickname
                          :img_location img_location}))

(s/defn ->Comment [{:keys [content id parent_comment_id created_at username nickname img_location]}]
  (let [r (-> {:content content
               :created_at created_at
               :parent_comment_id parent_comment_id
               :id id}
              (add-user username nickname img_location))]
    (if parent_comment_id
      r
      (dissoc r :parent_comment_id))))
                   
(s/defn ^:always-validate
  post-comments :- [sc/Comment]
  [db-spec post-id :- s/Num]
  (let [comments-flat (j/query db-spec ["SELECT c.ID, c.parent_comment_id, 
c.Content,
c.created_at,
u.Username, u.Nickname, u.Img_location
FROM blog.Comment c
JOIN blog.Users u ON u.ID = c.creator_id
WHERE c.parent_post_id = ?
ORDER BY c.created_at", post-id])
        root-comments (filter (comp nil? :parent_comment_id) comments-flat)
        children (map ->Comment
                      (filter :parent_comment_id comments-flat))]
    (map
     (comp
      (fn [{:keys [id] :as comment}]
        (-> comment
            (assoc :children (mapv
                              #(dissoc % :id :parent_comment_id)
                              (filter (fn [{:keys [parent_comment_id]}]
                                        (= parent_comment_id id)) children)))
            (dissoc :id)))
      (fn [comment]
        (->Comment comment)))
     root-comments)))

   
   
(defn ->Post [{:keys [username nickname img_location] :as db-row}]
  (-> db-row
      (add-user username nickname img_location)
      (dissoc :username :nickname :img_location)))

(s/defn ^:always-validate get-by-id :- sc/Commented-Post
  [{:keys [db-spec]} id]
  (let [db-row (j/query db-spec ["SELECT p.Title, p.created_at, p.Content, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS amount_of_comments
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE p.ID = ?
GROUP BY p.ID, u.ID" id] :result-set-fn first
                        :row-fn #(change-key % :amount_of_comments :amount-of-comments))]
    (-> db-row ->Post
        (assoc :comments (post-comments db-spec id)))))

(s/defn ^:always-validate get-all :- [sc/Post]
  [{:keys [db-spec]} limit :- (s/maybe s/Int)]
  (let [sql-vec (if limit
                  ["SELECT p.Title, p.Content, p.created_at, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS amount_of_comments
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
GROUP BY p.ID, u.ID
ORDER BY p.created_at DESC
LIMIT ?" limit]
                  ["SELECT p.Title, p.Content, p.created_at, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS amount_of_comments
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
GROUP BY p.ID, u.ID
ORDER BY p.created_at DESC"])]
    (j/query db-spec sql-vec :row-fn (comp #(change-key % :amount_of_comments :amount-of-comments)
                                           ->Post))))
