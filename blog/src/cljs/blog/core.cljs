(ns blog.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [re-frame.core :refer [dispatch]]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]

              [blog.settings :refer [settings]]
              [blog.state.handlers]
              [blog.main.post-widget :refer [default-post-widget]]))

;; -------------------------
;; Views

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/blog/" []
  (session/put! :current-page #'default-post-widget))

(secretary/defroute "/" []
  (session/put! :current-page #'default-post-widget))

;; -------------------------
;; Initialize app

(defn mount-root []  
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))

(dispatch [:load-page 1 (:recent-post-count settings)]) ;;page 1 amount-of-posts-per-page
