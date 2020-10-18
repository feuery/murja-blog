(ns blog.handler
  (:require [blog.server-conf :refer :all]
            [clojure.pprint :refer [pprint]]
            [blog.util :refer [destructure-db]]
            [schema.core :as s]
            [blog.config :refer [config]]
            [compojure.api.api :as api :refer [api]]
            [compojure.api.sweet :as sw :refer [context undocumented]]
            [compojure.api.core :as c :refer [GET POST PUT DELETE]]
            [ring.util.http-response :refer [internal-server-error ok]]
            [hiccup.page :refer [include-js include-css html5]]
            [ring.middleware.params :refer [wrap-params]]
            [blog.posts.routes :as post-r]
            [blog.posts.db :as post-db]
            [blog.login.routes :as login-routes]
            [blog.users.routes :as users]
            [blog.settings.routes :as settings]
            [blog.importer.routes :as imp]
            [config.core :refer [env]]
            [blog.system.current :refer :all]
            [compojure.route :as route]
            [blog.session :refer [wrap-app-session]]
            [blog.access :as access]))

(defn get-path [{:keys [params] :as rq}]
  (:* params))

(def app- (api  {:swagger {:ui "/api-docs"
                           :spec "/swagger.json"
                           :data {:info {:title "Feuer's clj-blog's backend"
                                         :description "Backend for managing blog posts and stuff"}
                                  :tags [{:name "posts"
                                          :description "Routes for managing posts"}
                                         {:name "login"
                                          :description "Routes for logging in and managing sessions"}
                                         {:name "users"
                                          :description "Routes for persisting user records"}
                                         {:name "settings"
                                          :description "Returns settings for the cljs client"}]}}}
                (context "/api" []
                         #'post-r/routes
                         #'login-routes/routes
                         #'users/routes
                         #'imp/routes
                         #'settings/routes)

                ;; (route/resources "/js/")

                (undocumented (route/resources "/blog/")
                              (GET "*" rq
                                :sys sys
                                (try
                                  (let [{:keys [js-route css-route]} @config
                                        path (get-path rq)
                                        post-meta (destructure-db [sys]
                                                                  (if-let [[_ id] (re-matches #"/blog/post/(\d+)" path)]
                                                                    (post-db/make-fb-meta-tags db id)))]
                                        ;(pprint {:metapost post-meta})
                                    (assert js-route)

                                    (ok (html5 {:xmlns:og "http://ogp.me/ns#"
                                                :xmlns:fb "http://www.facebook.com/2008/fbml"}
                                               (into [:head
                                                      (include-css css-route)
                                                      [:meta {:charset "UTF-8"}]
                                                      [:script (slurp js-route)]]
                                                     post-meta)
                                               [:body
                                                [:div#app]
                                                [:script
"Elm.Main.init({
        node: document.getElementById(\"app\")
    });"]])))
                                  (catch Throwable t
                                    (clojure.pprint/pprint {:error t})
                                    {:status 500
                                     :body "Unexpected error"}))))))

(def app
  (-> #'app-
      wrap-params
      wrap-app-session))
