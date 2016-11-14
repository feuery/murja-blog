(ns blog.util
  (:require [ring.util.http-response :refer [internal-server-error]]))

(defn change-key [map old-key new-key]
  (-> map
      (assoc new-key (get map old-key))
      (dissoc old-key)))

(defmacro destructure-db [[sys-symbol] & forms]
  `(let [{:keys [~'db]} ~sys-symbol]
     (if (and ~'db
              (:db-spec ~'db))
       (do
         ~@forms)
       (internal-server-error "Db is a lie"))))
