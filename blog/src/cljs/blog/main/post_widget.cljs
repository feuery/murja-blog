(ns blog.main.post-widget
  (:require [blog.settings :refer [settings]]
            [blog.state.post-subs]
            [re-frame.core :refer [subscribe]]))

(defn post-widget [page]
  [:article#page (pr-str page)])

;; this sets up the state
(defn default-post-widget []
  (let [page (subscribe [:current-page])]
    (fn []
      [post-widget @page])))
