(ns blog.state.effects
  (:require [re-frame.core :refer [dispatch reg-fx]]
            [blog.client :refer [GET POST DELETE]]))

(reg-fx :get
        (fn [{:keys [url dispatch-key]}]
          (GET url #(dispatch [dispatch-key %]))))

(reg-fx :post
        (fn [{:keys [url dispatch-key body]}]
          (POST url body #(dispatch [dispatch-key %]))))

(reg-fx :alert
        (fn [msg]
          (js/alert msg)))

(reg-fx :delete
        (fn [{:keys [url dispatch-key]}]
          (DELETE url #(dispatch [dispatch-key %]))))
