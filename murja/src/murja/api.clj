(ns murja.api
  (:require [murja.config :as config]
            [murja.api.users :as api.users]))

(defn get-client-settings [_]
  {:status 200
   :body (:client-config config/config)})

(defn get-users-is-empty [{:keys [db]}]
  {:pre [(some? db)]}
  {:status 200
   :content-type "application/transit+json"
   :body {:is-empty? (api.users/is-empty db)}})
  
