(ns blog.posts.db
  (:require [clojure.java.jdbc :as j]
            [clojure.string :as str]
            [clj-time.coerce :as c]
            [blog.config :as con]
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

(defn ;; ^:always-validate
  get-titles-by-year ;; :- [Timed-Title]
  [{:keys [db-spec] :as db} & {:keys [show-hidden?]
                        :or {show-hidden? false}}]
  (j/query db-spec
           [(if-not show-hidden?
              "SELECT p.Title, p.created_at, p.id, p.Tags
FROM blog.Post p
WHERE NOT p.tags ?? 'hidden'
ORDER BY p.created_at DESC"
              "SELECT p.Title, p.created_at, p.id, p.Tags
FROM blog.Post p
ORDER BY p.created_at DESC")] :row-fn (fn [{:keys [title created_at id tags]}]
                                       (let [created_at (c/from-sql-time created_at)
                                             year (t/year created_at)
                                             month (int->month
                                                    (t/month created_at))]
                                         {:Title title
                                          :Year year
                                          :Id id
                                          :Month month
                                          :Tags tags}))
           :result-set-fn vec))

(defn post-versions [{:keys [db-spec] :as db} post-id]
  (j/query db-spec ["SELECT version 
FROM blog.Post_History 
WHERE ID = ? AND NOT tags ?? 'hidden' 
ORDER BY version ASC" post-id]
           :row-fn :version
           :result-fn vec))
   
(defn ->Post [db {:keys [username id nickname img_location] :as db-row}]
  (let [result (-> db-row
                   (add-user username nickname img_location)
                   (assoc :versions (post-versions db id))
                   (dissoc :username :nickname :img_location))]
    (if-not (:version result)
      (assoc result :version nil)
      result)))

(s/defn get-next-prev-postids [{:keys [db-spec] :as db} id]
  (let [next-id (j/query db-spec ["SELECT p.ID 
FROM blog.Post p
WHERE p.ID < ? AND NOT p.tags ?? 'hidden'
LIMIT 1" id] :row-fn :id :result-set-fn first)
        prev-id (j/query db-spec ["SELECT p.ID 
FROM blog.Post p
WHERE p.ID > ? AND NOT p.tags ?? 'hidden'
LIMIT 1" id] :row-fn :id :result-set-fn first)]
    {:next next-id :prev prev-id}))
           
(defn get-by-id ;; :- sc/Commented-Post
  [{:keys [db-spec] :as db} id & {:keys [allow-hidden?] :or {allow-hidden? false}}]
  (let [sql (if-not allow-hidden?
              "SELECT p.ID, p.Title, p.created_at, p.Content, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS amount_of_comments
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE p.ID = ? AND NOT p.tags ?? 'hidden'
GROUP BY p.ID, u.ID"
              "SELECT p.ID, p.Title, p.created_at, p.Content, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS amount_of_comments
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE p.ID = ?
GROUP BY p.ID, u.ID")
        db-row (j/query db-spec [sql id] :result-set-fn first
                        :row-fn #(change-key % :amount_of_comments :amount-of-comments))]
    (if-not (empty? db-row)
      (let [{:keys [next prev]} (get-next-prev-postids db id)]
        (-> (->Post db db-row)
            (assoc :comments (post-comments db-spec id)
                   :next-post-id next
                   :prev-post-id prev)))
      {})))

(defn get-versioned-by-id
  [{:keys [db-spec] :as db} post-id post-version]
  (let [db-row (j/query db-spec ["SELECT p.ID, p.Title, p.created_at, p.Content, p.tags, u.Username, u.Nickname, u.Img_location, p.version, COUNT(c.ID) AS amount_of_comments
FROM blog.Post_History p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE p.ID = ? AND p.version = ? AND not tags ?? 'hidden'
GROUP BY p.ID, u.ID, p.title, p.created_at, p.Content, p.tags, u.Username, u.Nickname, u.Img_location, p.version" post-id post-version]
                        :result-set-fn first
                        :row-fn #(change-key % :amount_of_comments :amount-of-comments))]
    (if-not (empty? db-row)
      ;; That function is broken...
      (let [{:keys [next prev]} (get-next-prev-postids db post-id)]
        (-> (->Post db db-row)
            (assoc :comments (post-comments db-spec post-id)
                   :next-post-id next
                   :prev-post-id prev)))
      {})))

(s/defn  ^:always-validate
  get-all :- [sc/Post]
  [{:keys [db-spec] :as db} limit :- (s/maybe s/Int)]
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
                                           (partial ->Post db)))))

(defn get-page 
  [{:keys [db-spec] :as db}
   page 
   page-size 
   & {:keys [allow-hidden?]
      :or {allow-hidden? false}}]
  (j/query db-spec ["SELECT p.ID, p.Title, p.Content, p.created_at, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS amount_of_comments
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE (NOT p.tags ?? 'hidden') OR ?
GROUP BY p.ID, u.ID
ORDER BY p.created_at DESC
LIMIT ?
OFFSET ?" allow-hidden? page-size (* (dec page) page-size)] {:row-fn (comp #(change-key % :amount_of_comments :amount-of-comments)
                                                                          (partial ->Post db))}))
        
    

(defn landing-page-ids [db-c]
  (j/query db-c ["SELECT id
FROM blog.Post 
WHERE tags ?? 'landing-page' AND NOT tags ?? 'hidden'"]
           :row-fn :id
           :result-set-fn vec))

(defn remove-tag! [{:keys [db-spec] :as db} tag id]
  (try
    (if (string? id)
      (println "Id on näköjään string?!"))
    (if-let [tags (j/query db-spec ["SELECT tags FROM blog.Post WHERE id = ?" id] :row-fn :tags :result-set-fn first)]
    (let [new-tags (filterv (complement #(= % tag)) tags)]
      (j/update! db-spec :blog.Post
                 {:tags new-tags}
                 ["id = ?" id])))
    (catch Exception ex
      ;; #dbg
      ;; #break
      (println "Täää?")
      (pprint ex)
      (throw ex))))

(s/defn ^:always-validate save-post!
  [{:keys [db-spec] :as db}
   {:keys [_id] :as user}
   {:keys [title content tags] :as post} :- sc/New-post]
  (try
    (j/with-db-transaction [c db-spec]
      (if (in? tags "landing-page")
        (let [ids (landing-page-ids c)]
          (mapv (partial remove-tag! {:db-spec c} "landing-page")
                ids)))
      (j/insert! c :blog.post
                 [:Title :Content :creator_id :tags ]
                 [title content _id tags]))
    (catch Exception ex
      (pprint ex)
      (throw ex))))

(s/defn ^:always-validate edit-post! [{:keys [db-spec] :as db}
                                      {:keys [_id] :as user}
                                      {:keys [title content tags id]} :- sc/edited-post]
  (try
    (j/with-db-transaction [c db-spec]
      (if (in? tags "landing-page")
        (let [ids (landing-page-ids c)]
          (mapv (partial remove-tag! {:db-spec c} "landing-page")
                ids)))
      ;; move the old post to the history table
      (j/execute! db-spec ["
INSERT INTO blog.Post_History(ID, Title, Content, creator_id, tags, created_at, version)
SELECT p.ID, p.Title, p.Content, p.creator_id, p.tags, p.created_at, coalesce(MAX(ph.version), 0) + 1
FROM blog.Post p
LEFT JOIN blog.Post_History ph ON p.ID = ph.ID
WHERE p.id = ?
GROUP BY p.ID" id])
      (j/update! c :blog.Post
                 {:title title
                  :content content
                  :tags tags}
                 ["id = ?" id]))
    (catch Exception ex
      (pprint ex)
      (throw ex))))

(defn delete-by-id
  ([{:keys [db-spec] :as db}
   post-id]
   (j/delete! db-spec :blog.Post ["ID = ?" post-id]))
  ([{:keys [db-spec] :as db}
    post-id
    post-version]
   (j/delete! db-spec :blog.Post_History ["ID = ? AND version = ?" post-id post-version])))

(defn delete-comment-by-id
  [{:keys [db-spec] :as db}
   comment-id]
  (j/delete! db-spec :blog.Comment ["ID = ?" comment-id]))

(s/defn comment-post!
  [{:keys [db-spec] :as db}
   {:keys [_id] :as user}
   comment :- s/Str]
  (let [{:keys [content parent-post-id
                parent-comment-id]} comment]
    (j/insert! db-spec :blog.Comment
               [:parent_post_id :parent_comment_id :content :creator_id]
               [parent-post-id parent-comment-id content _id])))

(s/defn ^:always-validate
  get-landing-page :- sc/Post
  [{:keys [db-spec] :as db}]
  (j/query db-spec ["SELECT p.ID, p.Title, p.created_at, p.Content, p.tags, u.Username, u.Nickname, u.Img_location, COUNT(c.ID) AS \"amount-of-comments\"
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
LEFT JOIN blog.Comment c ON c.parent_post_id = p.ID
WHERE p.tags ?? 'landing-page' AND NOT p.tags ?? 'hidden'
GROUP BY p.ID, u.ID"] :result-set-fn #(or (first %) {})
           :row-fn (partial ->Post db)))

(s/defn get-landing-page-title :- sc/Landing-page-result
  [{:keys [db-spec] :as db}]
  (or (j/query db-spec ["SELECT p.Title, p.Id
FROM blog.Post p
WHERE p.tags ?? 'landing-page' AND NOT p.tags ?? 'hidden'"]
               :result-set-fn  first)
      {}))

(defn strtake [n s]
  (apply str (take n s)))

(defn Meta [prop contents]
  [:meta {:content contents
          :property prop}])

(defn make-fb-meta-tags [{:keys [db-spec] :as db} post-id]
  (let [post-id (if (string? post-id)
                  (Long/parseLong post-id)
                  post-id)
        {:keys [content title] :as post} (get-by-id db post-id)]

    [(Meta "og:description" (str/replace (strtake 200 content) #"\n" ""))
     (Meta "og:title" title)
     (Meta "og:site_name" (get-in @con/config [:client-config :blog-title]))]))
