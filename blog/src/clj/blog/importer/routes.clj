(ns blog.importer.routes
  (:require [schema.core :as s]
            [compojure.api.core :as c :refer [GET POST PUT DELETE]]
            [compojure.core :as cc]
            [compojure.api.sweet :as sw :refer [context]]
            [ring.middleware.multipart-params :as mp]
            [ring.util.http-response :refer [internal-server-error ok unauthorized]]
            [ring.swagger.upload :as upload]
            [clojure.pprint :refer :all]
            [blog.util :refer [destructure-db]]
            [blog.access :as access]
            [blog.importer.db :as db]
            [ring.swagger.upload :as upload]))

(def routes
  (context "/importer" []
           :sys sys
           :tags ["importer"]
           (POST "/atom" []
                 :multipart-params [file :- upload/TempFileUpload]
                 :middleware [upload/wrap-multipart-params]
                 (destructure-db [sys]
                                 ;; #dbg
                                 (let [filedata (get file :tempfile)]
                                   (try
                                     (println "Trying to import!")
                                     (if (db/import-atom! db (slurp filedata))
                                       (ok)
                                       (internal-server-error))
                                     (catch Exception ex
                                       (pprint ex)
                                       (internal-server-error))))))))
