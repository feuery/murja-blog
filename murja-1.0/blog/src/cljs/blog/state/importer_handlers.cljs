(ns blog.state.importer-handlers
  (:require [re-frame.core :refer [dispatch reg-event-db reg-event-fx trim-v]]
            [ajax.core :refer [POST]]))

(reg-event-fx
 :upload-atom-feed-file
 [trim-v]
 (fn [{:keys [db]} _]
   (let [data (js/FormData.)
         files (.-files (js/document.getElementById "upload-file"))]
     (doseq [fk (.keys js/Object files)]
       (.append data "file" (aget files fk)))
     {:post {:url "/api/importer/atom"
             :body data
             :file? true
             :dispatch-key :imported}
      :alert "Sent the file!"
      :db db})))

(reg-event-fx
 :imported
 (fn [{:keys [db]} _]
   {:alert "Imported!"
    :db db}))
         
