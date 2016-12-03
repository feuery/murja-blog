(ns blog.client
  (:require [ajax.core :refer [POST GET DELETE] :rename {GET aGET
                                                         POST aPOST
                                                         DELETE aDELETE}]))

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

(defn DELETE [url handler]
  (aDELETE url {:error-handler #(js/alert %)
                :handler handler
                :format :transit
                :response-format :transit})) 
