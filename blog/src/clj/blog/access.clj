(ns blog.access
  (:require [buddy.auth :refer [authenticated?]]
            [schema.core :as s]
            [blog.util :refer [in?]]
            [blog.system.current :refer [current-system]]
            [clojure.java.jdbc :as j]))

(defn authenticated [req]
  (authenticated? req))

(def nempty (complement empty?))


(s/defn can? [action :- (s/either s/Str s/Keyword)
              req]
  (if-let [{:keys [db]} @current-system]
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
           (filter nempty)
           nempty)
      (do
        (println "Can't find :groups from (:identity req)")
        (println "Has the user logged in?")
        false))
    (do
      (println "Can't find :db. Please start the system first")
      false)))
  

;; (defn admin [req]
;;   (and (authenticated? req)
;;        (in? (map :GroupId (:groups (:identity req))) 1)))
