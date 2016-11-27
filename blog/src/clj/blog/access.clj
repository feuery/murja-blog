(ns blog.access
  (:require [buddy.auth :refer [authenticated?]]
            [blog.util :refer [in?]]))

(defn authenticated [req]
  (authenticated? req))

(defn admin [req]
  (and (authenticated? req)
       (in? (map :GroupId (:groups (:identity req))) 1)))
