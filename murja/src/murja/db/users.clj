(ns murja.db.users
  (:require [clojure.java.jdbc :as j]
            [murja.security :refer [sha-512]]))

(defn update-user! [{:keys [db-spec]} {:keys [_id nickname img_location password]}]
  (j/update! db-spec :blog.Users (cond-> {:Nickname nickname
                                          :Img_location img_location
                                          :password (sha-512 password)}
                                   (empty? password) (dissoc :password)) ["ID = ?" _id]))

(defn everything-is-empty? [{:keys [db-spec]}]
  (= (j/query db-spec ["SELECT COUNT(*) AS Count FROM blog.Users"] {:row-fn :count
                                                                    :result-set-fn first}) 0))

(defn register-user! [{:keys [db-spec]} nickname username img_location password]
  (j/with-db-transaction [d db-spec]
    (let [everything-was-empty? (everything-is-empty? {:db-spec d})]

      (j/insert! d :blog.Users
                 [:Username :Nickname :Img_location :password]
                 [username nickname img_location (sha-512 password)])
      (let [user-id (j/query d ["SELECT u.id FROM blog.Users u WHERE u.Username = ?" username]
                             {:row-fn :id
                              :result-set-fn first})]
        (j/insert! d :blog.GroupMapping
                   [:UserID :GroupID :PrimaryGroup]
                   [user-id (if everything-was-empty?
                              1 ;; admin's id
                              2 ;; user's id
                              ) true])))))
