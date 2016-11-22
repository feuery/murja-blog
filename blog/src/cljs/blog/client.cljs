(ns blog.client
  (:require [ajax.core :refer [GET] :rename {GET aGET}]))

(defn GET [url handler]
  (aGET url {:headers {"Accept" "application/transit+json"}
             :error-handler #(js/alert %)
             :handler handler}))
