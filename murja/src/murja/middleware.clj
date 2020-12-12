(ns murja.middleware
  (:require [murja.db :as db]))

(defn wrap-db [handler]
  (fn [request]
    (handler (assoc request :db db/db)))) 
