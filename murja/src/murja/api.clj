(ns murja.api
  (:require [murja.config :as config]))

(defn get-client-settings [_]
  {:status 200
   :body (:client-config config/config)})
