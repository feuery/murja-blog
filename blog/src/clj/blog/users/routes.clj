(ns blog.users.routes
  (:require [schema.core :as s]
            [compojure.api.core :as c :refer [GET POST PUT DELETE]] 
            [compojure.api.sweet :as sw :refer [context]]
            [ring.util.http-response :refer [internal-server-error ok unauthorized]]
            [clojure.pprint :refer :all]
            [blog.util :refer [destructure-db]]
            [blog.access :as access]
            [blog.users.db :as db]
            
            [blog.login.schemas :refer :all]))

(def routes
  (context "/users" []
           :sys sys
           :tags ["users"]

           (POST "/save" []
                 :auth-rules access/authenticated
                 :body [{:keys [nickname img_location password] :as user-msg} new-user-message]
                 :return new-user-message
                 :current-user user
                 (let [{:keys [_id]} user]
                   (destructure-db [sys]
                                   (db/update-user! db _id nickname img_location password)
                                   (ok (dissoc user-msg :password)))))))
