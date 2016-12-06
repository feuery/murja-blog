(ns blog.state.importer-handlers
  (:require [re-frame.core :refer [dispatch reg-event-db reg-event-fx trim-v]]
            [ajax.core :refer [POST]]))

(reg-event-fx
 :upload-atom-feed-file
 [trim-v]
 (fn [{:keys [db]} _]
   ;; (let [form (js/document.getElementById "upload-file")
   ;;       _ (println (pr-str (.-files form)))
   ;;       file (aget (.-files form) 0)
   ;;       form-data (doto (js/FormData.)
   ;;                   (.append "file.xml" file))]
   (let [data {:file (js/FormData. (js/document.getElementById "import-form"))}]
     (js/alert (pr-str data)
     {:post {:url "/api/importer/atom"
             :body data
             :file? true
             :dispatch-key :imported}
      :db db}))));)

(reg-event-fx
 :imported
 (fn [{:keys [db]} _]
   {:alert "Imported!"
    :db db}))
         
