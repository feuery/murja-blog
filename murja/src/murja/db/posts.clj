(ns murja.db.posts
  (:require [clojure.java.jdbc :as j]
            clojure.pprint
            [hugsql.core :refer [def-db-fns]]
            [murja.specs.timed-title :as timed-title]
            [clojure.string :as str]
            [murja.config :as con]
            [muuntaja.core :as muuntaja]
            [clojure.set :refer [rename-keys]]
            #_[murja.util :refer :all])
  (:import [org.postgresql.util PGobject]))

(def-db-fns "post.sql")

(defn change-key [map old-key new-key]
  (rename-keys map {old-key new-key}))

(defn generate-string [obj]
  (->> obj
       (muuntaja/encode "application/json")
       slurp))

(defn parse-string [str]
  (->> str
       (muuntaja/decode "application/json")))

(extend-protocol j/ISQLValue
  clojure.lang.IPersistentMap
  (sql-value [value]
    (doto (PGobject.)
      (.setType "jsonb")
      (.setValue (generate-string value))))
  clojure.lang.PersistentVector
  (sql-value [value]
    (doto (PGobject.)
      (.setType "jsonb")
      (.setValue (generate-string value)))))

(extend-protocol j/IResultSetReadColumn
  PGobject
  (result-set-read-column [pgobj metadata idx]
    (let [type  (.getType pgobj)
          value (.getValue pgobj)]
      (case type
        "jsonb" (cond-> (parse-string value)
                  seq? vec)
        :else (do
                (println "Type " type " not recognized")
                value)))))

(defn add-user [row username nickname img_location]   
     (assoc row :creator {:username username
                          :nickname nickname
                          :img_location img_location}))

(defn ->Comment [{:keys [content id parent_comment_id created_at username nickname img_location]}]
  (let [r (-> {:content content
               :created_at created_at
               :parent_comment_id parent_comment_id
               :id id}
              (add-user username nickname img_location))]
    (if parent_comment_id
      r
      (dissoc r :parent_comment_id))))
                   
(defn
  post-comments
  [db-spec post-id]
  (let [comments-flat (post-comments* db-spec {:parent-post-id post-id})
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

(defn get-titles-by-year 
  [{:keys [db-spec] :as db} & {:keys [show-hidden?]
                               :or {show-hidden? false}}]
  {:pre [(some? db-spec)]}
  (try
    (->> (get-titles-by-year* db-spec {:show-hidden show-hidden?})
         (mapv (comp #(rename-keys % {:month :Month
                                      :id :Id
                                      :tags :Tags
                                      :year :Year
                                      :title :Title})
                     #(update % :month (comp timed-title/int->month int)))))
    (catch Exception ex
      (clojure.pprint/pprint {:db db})
      (throw ex))))
;; TODO make a real automated test of this
#_(assert (and (= (count (get-titles-by-year murja.db/db )) 21)
             (= (count (get-titles-by-year murja.db/db :show-hidden? true)) 22)))


(defn post-versions [{:keys [db-spec] :as db} post-id]
  (->> (post-versions* db-spec {:post-id post-id})
       (mapv :version)))
   
(defn ->Post [db {:keys [username id nickname img_location] :as db-row}]
  (let [result (-> db-row
                   (add-user username nickname img_location)
                   (assoc :versions (post-versions db id))
                   (dissoc :username :nickname :img_location))]
    (if-not (:version result)
      (assoc result :version nil)
      result)))

(defn get-next-prev-postids [{:keys [db-spec] :as db} id]
  (let [{next-id :id} (next-post-id db-spec {:post-id id})
        {prev-id :id} (prev-post-id db-spec {:post-id id})]
    {:next next-id :prev prev-id}))
           
(defn get-by-id [{:keys [db-spec] :as db} id & {:keys [allow-hidden?] :or {allow-hidden? false}}]
  (if-let [db-row (get-by-id* db-spec {:post-id id
                                       :show-hidden allow-hidden?})]
    (let [{:keys [next prev]} (get-next-prev-postids db id)]
      (-> (->Post db db-row)
          (assoc :comments (post-comments db-spec id)
                 :next-post-id next
                 :prev-post-id prev)))
    {}))

(defn get-versioned-by-id
  [{:keys [db-spec] :as db} post-id post-version]
  (if-let [db-row (get-versioned-by-id* db-spec {:post-id post-id
                                                 :version-id post-version})]
      (let [{:keys [next prev]} (get-next-prev-postids db post-id)]
        (-> (->Post db db-row)
            (assoc :comments (post-comments db-spec post-id)
                   :next-post-id next
                   :prev-post-id prev)))
      {}))


(defn get-all
  [{:keys [db-spec] :as db} limit]
  (->> (if limit
         (get-all* db-spec {:limit limit})
         (get-all* db-spec))
       (mapv (partial ->Post db))))

(defn get-page 
  [{:keys [db-spec] :as db}
   page 
   page-size 
   & {:keys [allow-hidden?]
      :or {allow-hidden? false}}]
  (->> (get-page* db-spec {:show-hidden  allow-hidden?
                           :page-size page-size
                           :page-id (* (dec page) page-size)})
       (mapv (partial ->Post db))))

;; (->> (get-page murja.db/db 2 2)
;;      (map :id))
;; (22 21)
;; (24 23)

        
    

(defn landing-page-ids [db-c]
  (->> (landing-page-ids* db-c)
       (mapv :id)))

(defn remove-tag! [{:keys [db-spec] :as db} tag id]
  (if-let [{:keys [tags]} (get-posts-tags* db-spec {:post-id id})]
    (let [new-tags (filterv (complement #(= % tag)) tags)]
      ;; tälle pitääkin sitten nyt oikeasti duunata automaagitestiä
      (update-tags* db-spec {:new-tags new-tags}))))

(defn edit-post! [{:keys [db-spec] :as db}
                  {:keys [_id] :as user}
                  {:keys [title content tags id] :as post}]

  (j/with-db-transaction [c db-spec]
    (if ((set tags) "landing-page")
      (let [ids (landing-page-ids c)]
        (mapv (partial remove-tag! {:db-spec c} "landing-page")
              ids)))
    (update-post db-spec post)))

(defn delete-by-id
  ([{:keys [db-spec] :as db}
    post-id]
   (delete-post db-spec {:id post-id}))
  ([{:keys [db-spec] :as db}
    post-id
    post-version]
   (delete-post db-spec {:id post-id
                         :version post-version})))

(defn delete-comment-by-id
  [{:keys [db-spec] :as db}
   comment-id]
  (delete-comment db-spec {:id comment-id}))

(defn comment-post!
  [{:keys [db-spec] :as db}
   {:keys [_id] :as user}
   comment]
  (let [{:keys [content parent-post-id
                parent-comment-id]} comment]
    (j/insert! db-spec :blog.Comment
               [:parent_post_id :parent_comment_id :content :creator_id]
               [parent-post-id parent-comment-id content _id])))

(defn
  get-landing-page
  [{:keys [db-spec] :as db}]
  (->> (get-landing-page* db-spec)
       (->Post db)))

(defn get-landing-page-title
  [{:keys [db-spec] :as db}]
  (or (landing-page-title db-spec)
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
     (Meta "og:site_name" (get-in con/config [:client-config :blog-title]))]))

(defn save-post!
  [{:keys [db-spec] :as db}
   {:keys [_id] :as user}
   {:keys [title content tags] :as post}]
  (j/with-db-transaction [c db-spec]
    (if ((set tags) "landing-page")
      (let [ids (landing-page-ids c)]
        (mapv (partial remove-tag! {:db-spec c} "landing-page")
              ids)))
    (insert-post db-spec (assoc post :creator-id _id))))
