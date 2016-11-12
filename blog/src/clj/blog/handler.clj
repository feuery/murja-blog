(ns blog.handler
  (:require [clojure.pprint :refer [pprint]]
            [compojure.api.api :as api :refer [defapi api]]
            [compojure.api.sweet :as sw :refer [context]]
            [compojure.api.meta :refer [restructure-param]]
            [compojure.api.core :as c :refer [GET POST PUT DELETE]] 
            [hiccup.page :refer [include-js include-css html5]]
            [blog.middleware :refer [wrap-middleware]]
            [blog.posts.routes :as post-r]
            [config.core :refer [env]]
            [blog.system.current :refer :all]))


#_((include-css (if (env :dev) "/css/site.css" "/css/site.min.css"))
   (include-js "/js/app.js"))

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

(declare db)

(defapi app {:swagger {:ui "/api-docs"
                       :spec "/swagger.json"
                       :data {:info {:title "Feuer's clj-blog's backend"
                                     :description "Backend for managing blog posts and stuff"}
                              :tags [{:name "posts"
                                      :description "Routes for managing posts"}]}}}
  (context "/api" []
           #'post-r/routes))
