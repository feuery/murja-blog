(ns blog.handler
  (:require [compojure.api.api :as api :refer [defapi api]]
            [hiccup.page :refer [include-js include-css html5]]
            [blog.middleware :refer [wrap-middleware]]
            [blog.posts.routes :as post-r]
            [config.core :refer [env]]))


#_((include-css (if (env :dev) "/css/site.css" "/css/site.min.css"))
   (include-js "/js/app.js"))

(defapi app {:swagger {:ui "/api-docs"
                       :spec "/swagger.json"
                       :data {:info {:title "Feuer's clj-blog's backend"
                                     :description "Backend for managing blog posts and stuff"}
                              :tags [{:name "posts"
                                      :description "Routes for managing posts"}]}}}
  #'post-r/routes)
