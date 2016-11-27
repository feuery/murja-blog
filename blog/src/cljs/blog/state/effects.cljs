(ns blog.state.effects
  (:require [re-frame.core :refer [dispatch reg-fx]]
            [blog.client :refer [GET POST]]))

(reg-fx :get
        (fn [{:keys [url dispatch-key]}]
          (GET url #(dispatch [dispatch-key %]))))

(reg-fx :post
        (fn [{:keys [url dispatch-key body]}]
          (POST url body #(dispatch [dispatch-key %]))))
