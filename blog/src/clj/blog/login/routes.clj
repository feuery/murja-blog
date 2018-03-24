(ns blog.login.routes
  (:require [schema.core :as s]
            [compojure.api.core :as c :refer [GET POST PUT DELETE]] 
            [compojure.api.sweet :as sw :refer [context]]
            [ring.util.http-response :refer [internal-server-error ok unauthorized]]
            [clojure.pprint :refer :all]
            [blog.util :refer [destructure-db]]
            [blog.access :as access]
            [blog.login.db :as db]


            [blog.login.schemas :refer :all]))

(def routes
  (context "/login" []
           :sys sys
           :tags ["login"]
           (POST "/login" rq
                 :body [{:keys [username password]} {:username s/Str
                                                     :password s/Str}]
                 (destructure-db [sys]
                                 (if-let [login-data (db/authenticate? db username password)]
                                   (do
                                     (println username " logged in!")
                                     (assoc-in (ok login-data) [:session :identity] {:_id (:userid login-data)
                                                                                     :groups (db/user-groups db username)}))
                                   (do
                                     (println (pr-str username) " failed to log in! Have the groups been set up correctly?")
                                     (unauthorized)))))
           (POST "/logout" []
                 (assoc-in (ok {}) [:session :identity] nil))

           (GET "/session" []
                :auth-rules access/authenticated
                :current-user user
                :return (s/maybe login-response)
                (destructure-db [sys]
                                (ok (db/get-user-view-data db (:_id user)))))))
