(ns blog.posts.db
  (:require [clojure.java.jdbc :as j]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [schema.core :as s]
            [blog.posts.schemas :as sc]
            [blog.util :refer :all]
            [clojure.pprint :refer :all]
            [cheshire.core :as ch]
            [blog.date-schemas :refer [Timed-Title int->month]])
  (:import [org.postgresql.util PGobject]))

(extend-protocol j/ISQLValue
  clojure.lang.IPersistentMap
  (sql-value [value]
    (doto (PGobject.)
      (.setType "jsonb")
      (.setValue (ch/generate-string value))))
  clojure.lang.PersistentVector
  (sql-value [value]
    (doto (PGobject.)
      (.setType "jsonb")
      (.setValue (ch/generate-string value)))))

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
                                        (= parent_comment_id id)) children)))))
      (fn [comment]
        (->Comment comment)))
     root-comments)))

(s/defn ^:always-validate
  get-titles-by-year :- [Timed-Title]
  [{:keys [db-spec]}]
  (j/query db-spec
           ["SELECT p.Title, p.created_at, p.id
FROM blog.Post p
WHERE NOT p.tags ?? 'hidden'
ORDER BY p.created_at DESC"] :row-fn (fn [{:keys [title created_at id]}]
                                       (let [created_at (c/from-sql-time created_at)
                                             year (t/year created_at)
                                             month (int->month
                                                    (t/month created_at))]
                                         {:Title title
                                          :Year year
                                          :Id id
                                          :Month month}))
           :result-set-fn vec))                                       
   
(defn ->Post [{:keys [username nickname img_location] :as db-row}]
  (-> db-row
      (add-user username nickname img_location)
      (dissoc :username :nickname :img_location)))

(s/defn get-next-prev-postids [{:keys [db-spec]} id]
  (let [next-id (j/query db-spec ["SELECT p.ID 
FROM blog.Post p
WHERE p.ID < ? AND NOT p.tags ?? 'hidden'
LIMIT 1" id] :row-fn :id :result-set-fn first)
        prev-id (j/query db-spec ["SELECT p.ID 
FROM blog.Post p
WHERE p.ID > ? AND NOT p.tags ?? 'hidden'
LIMIT 1" id] :row-fn :id :result-set-fn first)]
    {:next next-id :prev prev-id}))
           
(s/defn ^:always-validate
  get-by-id :- sc/Commented-Post
  [{:keys [db-spec] :as db} id]
  (let [db-row (j/query db-spec ["SELECT p.ID, p.Title, p.created_at, p.Content, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS amount_of_comments
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE p.ID = ? AND NOT p.tags ?? 'hidden'
GROUP BY p.ID, u.ID" id] :result-set-fn first
                        :row-fn #(change-key % :amount_of_comments :amount-of-comments))]
    (if-not (empty? db-row)
      (let [{:keys [next prev]} (get-next-prev-postids db id)]
        (-> db-row ->Post
            (assoc :comments (post-comments db-spec id)
                   :next-post-id next
                   :prev-post-id prev)))
      {})))

(s/defn  ^:always-validate
  get-all :- [sc/Post]
  [{:keys [db-spec]} limit :- (s/maybe s/Int)]
  (let [sql-vec (if limit
                  ["SELECT p.id, p.Title, p.Content, p.created_at, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS amount_of_comments
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE NOT p.tags ?? 'hidden'
GROUP BY p.ID, u.ID
ORDER BY p.created_at DESC
LIMIT ?" limit]
                  ["SELECT p.Id, p.Title, p.Content, p.created_at, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS amount_of_comments
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE NOT p.tags ?? 'hidden'
GROUP BY p.ID, u.ID
ORDER BY p.created_at DESC
"])]
    (j/query db-spec sql-vec :row-fn (comp #(change-key % :amount_of_comments :amount-of-comments)
                                           ->Post))))

(s/defn ^:always-validate get-page :- [sc/Post]
  [{:keys [db-spec]}
   page :- s/Num
   page-size :- s/Num]
  (j/query db-spec ["SELECT p.ID, p.Title, p.Content, p.created_at, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS amount_of_comments
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE NOT p.tags ?? 'hidden'
GROUP BY p.ID, u.ID
ORDER BY p.created_at DESC
LIMIT ?
OFFSET ?" page-size (* (dec page) page-size)] :row-fn (comp #(change-key % :amount_of_comments :amount-of-comments)
                                                            ->Post)))

(s/defn ^:always-validate save-post!
  [{:keys [db-spec]}
   {:keys [_id] :as user}
   {:keys[title content tags] :as post} :- sc/New-post]
  (j/insert! db-spec :blog.post
             [:Title :Content :creator_id :tags ]
             [title content _id tags]))

(defn delete-by-id
  [{:keys [db-spec]}
   post-id]
  (j/delete! db-spec :blog.Post ["ID = ?" post-id]))

(defn delete-comment-by-id
  [{:keys [db-spec]}
   comment-id]
  (j/delete! db-spec :blog.Comment ["ID = ?" comment-id]))

(s/defn comment-post!
  [{:keys [db-spec]}
   {:keys [_id] :as user}
   comment :- s/Str]
  (let [{:keys [content parent-post-id
                parent-comment-id]} comment]
    (j/insert! db-spec :blog.Comment
               [:parent_post_id :parent_comment_id :content :creator_id]
               [parent-post-id parent-comment-id content _id])))

(s/defn ^:always-validate
  get-landing-page :- sc/Post
  [{:keys [db-spec]}]
  (j/query db-spec ["SELECT p.ID, p.Title, p.created_at, p.Content, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS \"amount-of-comments\"
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE p.tags ?? 'landing-page' AND NOT p.tags ?? 'hidden'
GROUP BY p.ID, u.ID"] :result-set-fn #(or (first %) {})
           :row-fn ->Post))

(s/defn get-landing-page-title :- sc/Landing-page-result
  [{:keys [db-spec]}]
  (or (j/query db-spec ["SELECT p.Title, p.Id
FROM blog.Post p
WHERE p.tags ?? 'landing-page' AND NOT p.tags ?? 'hidden'"]
               :result-set-fn  first)
      {}))
