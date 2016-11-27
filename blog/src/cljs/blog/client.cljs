(ns blog.client
  (:require [ajax.core :refer [POST GET] :rename {GET aGET
                                                  POST aPOST}]))

(defn GET [url handler]
  (aGET url {:error-handler #(js/alert %)
             :format :transit
             :response-format :transit
             :handler handler}))

(defn POST [url body handler]
  (aPOST url {:error-handler #(js/alert %)
              :format :transit
              :response-format :transit
              :handler handler
              :params (or body {})}))
