(ns murja.api
  (:require [murja.config :as config]
            [murja.api.posts :as api.posts]
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

(defn get-session [{:keys [db]
                    {:keys [_id]} :user}]
  {:status 200
   :body (api.login/get-user-view-data db _id)})

(defn get-posts-titles [{:keys [db]}]
  {:status 200
   :body (api.posts/get-titles-by-year db :show-hidden? false)})

(defn get-posts-all-titles [{:keys [db]}]
  {:status 200
   :body (api.posts/get-titles-by-year db :show-hidden? true)})


(defn get-existing-landing-page [{:keys [db]}]
  {:status 200
   :body (api.posts/get-existing-landing-page db)})

(defn get-post-version [{:keys [db parameters]}]
  (let [{{post-id :id
         post-version :version} :path} parameters]

    {:status 200
     :body (api.posts/get-post-version-by-id db post-id post-version)}))

(defn get-post-id [{:keys [db parameters]}]
  (let [{{post-id :id} :path} parameters]

    {:status 200
     :body (api.posts/get-post-by-id db post-id :show-hidden? false)}))

(defn get-post-id-allow-hidden [{:keys [db parameters]}]
  (let [{{post-id :id} :path} parameters]
    {:status 200
     :body (api.posts/get-post-by-id db post-id :show-hidden? true)}))

(defn get-id-versions [{:keys [db parameters]}]
  (let [{{post-id :id} :path} parameters]
    {:status 200
     :body (api.posts/get-post-versions db post-id)}))
