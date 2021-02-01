(ns murja.db.login
  (:require [clojure.java.jdbc :as j]
            [murja.security :refer [sha-512]]
            #_[muuntaja.core :as muuntaja])
  (:import [org.postgresql.util PGobject]))

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
  (j/query db-spec ["select distinct perm.action
from blog.users u
join blog.groupmapping gm on u.id = gm.userid
join blog.grouppermissions gp on gp.groupid = gm.groupid
join blog.permission perm on gp.permissionid = perm.id
where u.id = ? OR u.username = ?" userid username] {:row-fn :action}))


(defn authenticate? [{:keys [db-spec] :as db} username passwd]
  (let [users (j/query db-spec ["SELECT u.Username, u.Nickname, u.ID as UserID, u.Password, u.Img_location, ug.ID as GroupID, ug.Name as GroupName, gm.PrimaryGroup
FROM blog.Users u
JOIN blog.GroupMapping gm ON u.ID = gm.UserID
JOIN blog.UserGroup ug ON ug.ID = gm.GroupID
WHERE u.Username = ? AND u.Password = ?" username (sha-512 passwd)])]
    (if (empty? users)
      nil
      (let [primary-group (filter :primarygroup users)]
        (if-not (= (count primary-group) 1)
          (do
            (println "User must have 1 primary group. " username " had " (count primary-group))
            nil)
          (let [{:keys [nickname groupname img_location userid username]} (first primary-group)]
            {:nickname nickname
             :username username
             :img_location img_location
             :userid userid
             :primary-group-name groupname
             :permissions (user-permissions db :username username)}))))))

(defn get-user-view-data [{:keys [db-spec] :as db} user-id]
  (j/query db-spec ["SELECT u.Username, u.Nickname, u.Img_location, ug.Name as GroupName, gm.PrimaryGroup, u.ID as UserID
FROM blog.Users u
JOIN blog.GroupMapping gm ON u.ID = gm.UserID
JOIN blog.UserGroup ug ON ug.ID = gm.GroupID
where u.ID = ?" user-id]
           {:row-fn (fn [{:keys [nickname username groupname img_location userid] :as user}]
                      {:nickname nickname
                       :username username
                       :img_location img_location
                       :userid userid
                       :primary-group-name groupname
                       :permissions (user-permissions db :username username)})
           :result-set-fn first}))

(defn user-groups
  [{:keys [db-spec]} username]
  (j/query db-spec ["SELECT ug.ID, ug.Name, ug.Description
FROM blog.Users u
LEFT JOIN blog.GroupMapping um ON um.UserID = u.ID
LEFT JOIN blog.UserGroup ug ON um.GroupID = ug.ID
WHERE u.Username = ?" username]))


(defn can? [db
            action ;;:- (s/either s/Str s/Keyword)
            req]
  (if-let [groups (get-in req [:identity :groups])]
    (->> groups
         (map (fn [{:keys [id]}]
                {:pre [(some? id)]}
                (j/query (:db-spec db)
                         ["SELECT perm.action
FROM blog.GroupPermissions gp 
LEFT JOIN blog.Permission perm ON gp.PermissionID = perm.ID
WHERE gp.GroupID = ? AND perm.action = ?" id (if (keyword? action)
                                               (name action)
                                               (str action))])))
         (filter not-empty)
         not-empty)
    (do
      (println "Can't find :groups from (:identity req)")
      (println "Has the user logged in?")
      false)))
