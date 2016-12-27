(ns blog.settings.routes
  (:require [schema.core :as s]
            [compojure.api.core :as c :refer [GET POST PUT DELETE]] 
            [compojure.api.sweet :as sw :refer [context]]
            [ring.util.http-response :refer [internal-server-error ok]]
            [clojure.pprint :refer :all]
            [blog.posts.schemas :as post-sc]
            [blog.posts.db :as pdb]
            [blog.util :refer [destructure-db]]
            [blog.config :refer [config]]
            [blog.access :refer [can?]]))

(def routes
  (context "/settings" []
           :sys sys
           :tags ["settings"]
           (GET "/client-settings" []
                (ok (:client-config @config)))))
