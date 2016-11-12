(ns blog.server-conf
  (:require [compojure.api.meta :refer [restructure-param]]
            [blog.system.current :refer [current-system]]))

(defmethod restructure-param :sys [_ _
                                   acc]
  (loop [[[k v] & rst] (seq @current-system)
         acc acc]
    (if k
      (if (= k :server)
        (recur rst acc)
        (recur rst
               (update-in acc [:lets] into [(symbol (name k)) `(get @current-system ~k)])))
      acc)))
