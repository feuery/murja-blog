(ns murja.middleware
  (:require [murja.db :as db]
            [clojure.pprint]
            [murja.db.login :as db.login]
            #_[murja.db.users :as db.users]))

(defn wrap-db [handler]
  (fn [request]
    (handler (assoc request :db db/db))))

(defn wrapping-user [handler {:keys [db]
                              {{:keys [_id] :as req-identity} :identity} :session
                              :as request}]
  (if req-identity
    (handler (assoc request :user (merge req-identity (db.login/get-user-view-data db _id))))
    {:status 401
     :body "Unauthorized!"}))
  
(defn wrap-user [handler]
  (fn [req]
    (wrapping-user handler req)))

(defn do-can [handler {:keys [db user session] :as request} action]
  (assert (some? db) "db is nil, please use murja.middleware/wrap-db before can? middleware")
  (assert (some? user) "user is nil, this is either due to not being logged in or forgetting wrap-user before this middleware")

  (if (db.login/can? db action session)
    (handler request)
    {:status 401
     :body "Unauthorized!"}))

(defn can? [handler action]
  (fn [request]
    (do-can handler request action)))
