(ns blog.handler
  (:require [blog.server-conf :refer :all]
            [clojure.pprint :refer [pprint]]
            [compojure.api.api :as api :refer [defapi api]]
            [compojure.api.sweet :as sw :refer [context undocumented]]
            [compojure.api.core :as c :refer [GET POST PUT DELETE]]
            [ring.util.http-response :refer [internal-server-error ok]]
            [hiccup.page :refer [include-js include-css html5]]
            [ring.middleware.params :refer [wrap-params]]
            [blog.posts.routes :as post-r]
            [blog.login.routes :as login-routes]
            [blog.users.routes :as users]
            [blog.importer.routes :as imp]
            [config.core :refer [env]]
            [blog.system.current :refer :all]
            [compojure.route :as route]
            [blog.session :refer [wrap-app-session]]
            [blog.access :as access]))

(defapi app- {:swagger {:ui "/api-docs"
                       :spec "/swagger.json"
                       :data {:info {:title "Feuer's clj-blog's backend"
                                     :description "Backend for managing blog posts and stuff"}
                              :tags [{:name "posts"
                                      :description "Routes for managing posts"}
                                     {:name "login"
                                      :description "Routes for logging in and managing sessions"}
                                     {:name "users"
                                      :description "Routes for persisting user records"}]}}}
  (context "/api" []
           #'post-r/routes
           #'login-routes/routes
           #'users/routes
           #'imp/routes)

  (undocumented #_(route/resources "/css/")
                
                (GET "/" []
                     (ok
                      (html5 [:head
                              (include-css (if (env :dev) "/css/site.css" "/css/site.min.css"))]
                             [:body
                              [:div#app
                               [:p "This site requires js (at least until the lazy developer makes a server-side version of this clojurescript site"]
                               [:p "If you're dev, run `lein figwheel` in the project dir"]]
                              (include-js "/js/app.js")])))
                (route/resources "/")))

(def app
  (-> app-
      wrap-params
      wrap-app-session))
