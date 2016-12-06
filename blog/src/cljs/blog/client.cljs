(ns blog.client
  (:require [ajax.core :refer [POST PUT GET DELETE] :rename {GET aGET
                                                             POST aPOST
                                                             PUT aPUT
                                                             DELETE aDELETE}]))

(defn GET [url handler]
  (aGET url {:error-handler #(js/alert %)
             :format :transit
             :response-format :transit
             :handler handler}))

(defn POST [url body handler file?]
  (aPOST url (cond-> {:error-handler #(js/alert %)
                      :format :transit
                      :response-format :transit
                      :handler handler
                      (if file?
                        :body
                        :params) (or body {})}
               file? (dissoc :format :response-format))))

(defn PUT [url body handler]
  (aPUT url {:error-handler #(js/alert %)
             :format :transit
             :response-format :transit
             :handler handler
             :params (or body {})}))

(defn DELETE [url handler]
  (aDELETE url {:error-handler #(js/alert %)
                :handler handler
                :format :transit
                :response-format :transit})) 
