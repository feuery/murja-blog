(ns blog.login.db
  (:require [clojure.java.jdbc :as j]
            [schema.core :as s]
            [blog.util :refer :all]
            [blog.security :refer [sha-512]]
            [clojure.pprint :refer :all]
            [cheshire.core :as ch]
            [blog.login.schemas :refer [login-response Permission]])
  (:import [org.postgresql.util PGobject]))

(defn user-permissions 
  [{:keys [db-spec]} & {:keys [username userid] :or {username nil
                                                     userid nil}}]
  {:pre [(xor username
              userid)]
   ;; binding syntax was too much for poor schema.core
   :post [(s/validate [Permission] %)]}
  (j/query db-spec ["select distinct perm.action
from blog.users u
join blog.groupmapping gm on u.id = gm.userid
join blog.grouppermissions gp on gp.groupid = gm.groupid
join blog.permission perm on gp.permissionid = perm.id
where u.id = ? OR u.username = ?" userid username] :row-fn :action))


(s/defn ^:always-validate
  authenticate? :- (s/maybe login-response)
  [{:keys [db-spec] :as db} username passwd]
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
          (let [{:keys [nickname groupname img_location userid]} (first primary-group)]
            {:nickname nickname
             :img_location img_location
             :userid userid
             :primary-group-name groupname
             :permissions (user-permissions db :username username)}))))))

(defn user-groups
  [{:keys [db-spec]} username]
  (j/query db-spec ["SELECT ug.ID, ug.Name, ug.Description
FROM blog.Users u
LEFT JOIN blog.GroupMapping um ON um.UserID = u.ID
LEFT JOIN blog.UserGroup ug ON um.GroupID = ug.ID
WHERE u.Username = ?" username]))
