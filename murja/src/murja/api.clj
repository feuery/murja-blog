(ns murja.api
  (:require [murja.config :as config]
            [murja.api.users :as api.users]
            [murja.api.login :as api.login]))

(defn get-client-settings [_]
  {:status 200
   :body (:client-config config/config)})

(defn get-users-is-empty [{:keys [db]}]
  {:pre [(some? db)]}
  {:status 200
   :content-type "application/transit+json"
   :body {:is-empty? (api.users/is-empty db)}})

(defn post-users-save [{:keys [db session]
                        {new-user :body} :parameters}]
  (api.users/save-user db (merge (:identity session)
                                 new-user))
  {:status 200
   :body (dissoc new-user :password)})

(defn post-login [{:keys [db session]
                   {login-user :body} :parameters}]
  (if-let [login-data (api.login/do-login db login-user)]
    {:status 200
     :body login-data
     :session (assoc session :identity {:_id (:userid login-data)
                                        :groups (api.login/user-groups db login-user)}) }
    {:status 401
     :body "Unauthorized"}))

(defn post-logout [_]
  {:status 204
   :session nil})