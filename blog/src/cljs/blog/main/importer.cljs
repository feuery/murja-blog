(ns blog.main.importer
  (:require [reagent.core :as r]
            [blog.util :refer [value-of]]
            blog.state.importer-handlers
            [re-frame.core :refer [subscribe dispatch]]))

(defn import-gui []
  [:form#import-form {:enc-type "multipart/form-data"
                      :method "POST"}
   [:label "Give me an atom feed file to import content from"]
   [:input#upload-file {:type "file"
            :name "upload-file"}]
   [:button {:on-click #(do
                          (dispatch [:upload-atom-feed-file])
                          (.preventDefault %))}
    "Upload!"]])
   
