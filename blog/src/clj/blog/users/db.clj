(ns blog.users.db
  (:require [clojure.java.jdbc :as j]
            [schema.core :as s]
            [blog.util :refer :all]
            [blog.security :refer [sha-512]]
            [clojure.pprint :refer :all]
            [cheshire.core :as ch]))

(defn update-user! [{:keys [db-spec]} _id nickname img_location password]
  (j/update! db-spec :blog.Users (cond-> {:Nickname nickname
                                          :Img_location img_location
                                          :password (sha-512 password)}
                                   (empty? password) (dissoc :password)) ["ID = ?" _id]))
