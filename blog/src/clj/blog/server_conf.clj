(ns blog.server-conf
  (:require [compojure.api.meta :refer [restructure-param]]
            [blog.system.current :refer [current-system]]))

(defmethod restructure-param :sys [_ sym acc]
  (update-in acc [:lets] into [sym `(deref current-system)]))
