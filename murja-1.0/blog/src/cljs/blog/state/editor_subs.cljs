(ns blog.state.editor-subs
  (:require [re-frame.core :refer [reg-sub]]))

(reg-sub :edited-post
         (fn [db _]
           (:edited-post db)))
