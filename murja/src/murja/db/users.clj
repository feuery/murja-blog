(ns murja.db.users
  (:require [clojure.java.jdbc :as j]
            [hugsql.core :refer [def-db-fns]]
            [murja.security :refer [sha-512]]))

(def-db-fns "user.sql")

;; this is called from http-api /api/users/save
(defn update-user! [{:keys [db-spec]} {:keys [_id nickname img_location password] :as user}]
  (update-user* db-spec (if (empty? password)
                          (dissoc user :password)
                          (update user :password sha-512))))

;; this is called from http-api /api/users/save
(defn everything-is-empty? [{:keys [db-spec]}]
  (zero? (:count (count-users db-spec))))

;; this seems to be called from nowhere but repl 
(defn register-user! [{:keys [db-spec]} nickname username img_location password]
  (j/with-db-transaction [d db-spec]
    (let [everything-was-empty? (everything-is-empty? {:db-spec d})
          user {:nickname nickname
                :username username
                :img_location img_location
                :password password}]
                
      (insert-user* d (update user :password sha-512))
      (let [{user-id :id} (get-user d {:username username})]
        (insert-groupmapping d {:userid user-id
                                :groupid (if everything-was-empty?
                                           1 ;; admin's id
                                           2 ;; user's id
                                           )
                                :primarygroup true})))))
