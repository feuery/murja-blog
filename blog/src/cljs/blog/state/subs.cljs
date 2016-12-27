(ns blog.state.subs
  (:require [re-frame.core :refer [reg-sub]]))

(reg-sub :settings
         (fn [db _]
           (or (get db :settings)
               {})))
