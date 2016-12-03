(ns blog.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [re-frame.core :refer [dispatch dispatch-sync subscribe]]
              [re-frame.db :refer [app-db]]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]

              [blog.settings :refer [settings]]
              [blog.devtool :refer [devtool]]
              [blog.state.handlers]
              [blog.state.devtool-subs]
              [blog.main.post-widget :refer [default-post-widget]]
              [blog.main.single-post :refer [single-post-widget]]
              [blog.main.editor :refer [editor-container editor-sidebar-container]]
              blog.state.editor-subs
              blog.state.single-post-handlers
              blog.state.single-post-subs
              [blog.loginview :refer [loginview]]))

;; -------------------------
;; Views

(defn current-page []
  [:div#container
   [(session/get :current-main)]
   [:div#sidebar
    [(session/get :current-sidebar)]]])

;; -------------------------
;; Routes

(secretary/defroute "/blog/" []
  (dispatch [:load-page 1 (:recent-post-count settings)])
  (session/put! :current-sidebar #'loginview)
  (session/put! :current-main #'default-post-widget))

(secretary/defroute "/blog/post/:id" {:keys [id]}
  (println "JEEEE " id)
  (dispatch [:load-post-full (js/parseInt id)])
  (session/put! :current-main #'single-post-widget))

(secretary/defroute "/blog/create-post" []
  (dispatch [:start-new-post])
  (session/put! :current-main #'editor-container)
  (session/put! :current-sidebar #'editor-sidebar-container))

(secretary/defroute "/" []
  (secretary/dispatch! "/blog/"))

;; -------------------------
;; Initialize app

(defn mount-root []
  (let [target (.getElementById js/document "app")]
    (reagent/render [current-page] target)))

;;Called in $blogroot/env/dev/cljs/blog/dev.cljs
(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root)
  (js/console.log "init!"))

