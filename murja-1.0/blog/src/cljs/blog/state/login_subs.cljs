(ns blog.state.login-subs
  (:require [re-frame.core :refer [reg-sub]]))

(reg-sub :current-user
         (fn [db _]
           (get db :current-user)))

(reg-sub :is-empty?
         (fn [db _]
           (get db :is-empty?)))
