(ns blog.state.effects
  (:require [re-frame.core :refer [dispatch reg-fx]]
            [blog.client :refer [GET]]))

(reg-fx :get
        (fn [{:keys [url dispatch-key]}]
          (GET url #(dispatch [dispatch-key %]))))
