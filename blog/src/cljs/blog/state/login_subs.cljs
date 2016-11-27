(ns blog.state.login-subs
  (:require [re-frame.core :refer [reg-sub]]))

(reg-sub :current-user
         (fn [db _]
           (get db :current-user)))
