(ns blog.server-conf
  (:require [compojure.api.meta :refer [restructure-param]]
            [blog.system.current :refer [current-system]]
            [blog.session :refer [wrap-rule]]
            [blog.access :as access]))

(defmethod restructure-param :sys [_ sym acc]
  (update-in acc [:lets] into [sym `(deref current-system)]))

(defmethod restructure-param :auth-rules
  [_ rule acc]
  (update-in acc [:middleware] conj [wrap-rule rule]))

(defmethod restructure-param :current-user
  [_ binding acc]
  (update-in acc [:letks] into [binding `(:identity ~'+compojure-api-request+)]))
