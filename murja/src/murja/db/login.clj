(ns murja.db.login
  (:require [hugsql.core :refer [def-db-fns]]
            [murja.security :refer [sha-512]]
            #_[muuntaja.core :as muuntaja])
  (:import [org.postgresql.util PGobject]))

(def-db-fns "login.sql")

(defmacro xor 
  ([] nil)
  ([a] a)
  ([a b]
    `(let [a# ~a
           b# ~b]
      (if a# 
        (if b# nil a#)
        (if b# b# nil)))))

(defn user-permissions 
  [{:keys [db-spec]} & {:keys [username userid] :or {username nil
                                                     userid nil}}]
  {:pre [(xor username
              userid)]}
  (->> (user-permissions* db-spec {:userid userid
                                   :username username})
       (mapv :action)))


(defn authenticate? [{:keys [db-spec] :as db} username passwd]
  (if-let [{:keys [nickname groupname img_location userid username] :as user} (query-users* db-spec {:username username
                                                                                                     :password (sha-512 passwd)})]
    {:nickname nickname
     :username username
     :img_location img_location
     :userid userid
     :primary-group-name groupname
     :permissions (user-permissions db :username username)}))

(defn get-user-view-data [{:keys [db-spec] :as db} user-id]
  (let [view-data (get-user-view-data* db-spec {:user-id user-id})
        user (-> view-data
                first
                (dissoc :action))]
    (assoc user :permissions (mapv :action view-data))))

(defn user-groups
  [{:keys [db-spec]} username]
  (user-groups* db-spec {:username username}))


(defn can? [{:keys [db-spec]}
            action ;;:- (s/either s/Str s/Keyword)
            req]
  (if-let [groups (get-in req [:identity :groups])]
    (->> groups
         (map (fn [{:keys [id]}]
                {:pre [(some? id)]}
                (can?* db-spec {:group-id id
                                :action (if (keyword? action)
                                          (name action)
                                          (str action))})))
         (map :can?)
         (some true?))
    (do
      (println "Can't find :groups from (:identity req)")
      (println "Has the user logged in?")
      false)))
