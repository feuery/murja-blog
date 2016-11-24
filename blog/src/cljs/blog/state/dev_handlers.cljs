(ns blog.state.dev-handlers
  (:require [re-frame.core :refer [dispatch reg-event-db reg-event-fx trim-v]]
            [cljs.reader :refer [read-string]]))

(reg-event-db
 :toggle-devtool-visibility
 (fn [db _]
   (update db :show-devtool? not)))

(reg-event-db
 :set-db
 [trim-v]
 (fn [db [path val -type]]
   (try
     (let [val (read-string val)]
       (if-not (= (type val) -type)
         (do
           (js/console.error "Type mismatch in :set-db handler. Things are likely to break")
           db)
         (assoc-in db path val)))
     (catch :default ex
       (js/console.error (str "Error in set-db: " (pr-str ex)))
       db))))
