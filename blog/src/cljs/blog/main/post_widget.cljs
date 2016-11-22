(ns blog.main.post-widget
  (:require [blog.settings :refer [settings]]))

(defn post-widget [post-count]
  (fn [post-count]
    [:div "Näytän viimeisimmät " post-count " postia!"]))

(defn default-post-widget []
  [post-widget (:recent-post-count settings)])
