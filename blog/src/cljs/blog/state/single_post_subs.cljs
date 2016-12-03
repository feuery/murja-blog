(ns blog.state.single-post-subs
  (:require [re-frame.core :refer [reg-sub]]))

(reg-sub :selected-post
         (fn [db _]
           (get db :selected-post)))
