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
              [blog.main.editor :refer [editor-container]]
              blog.state.editor-subs))

;; -------------------------
;; Views

(defn current-page []
  (let [devtool-vis? (subscribe [:devtool-visible?])
        editor-visible? (subscribe [:editor-visible?])]
    (fn []
      [:div
       #_[:p "Hello world! " (pr-str @editor-visible?)]
       (if @editor-visible?
         [editor-container @editor-visible?]
         [(session/get :current-page)])
       [devtool @app-db @devtool-vis?]])))

;; -------------------------
;; Routes

(secretary/defroute "/blog/" []
  (session/put! :current-page #'default-post-widget))

(secretary/defroute "/" []
  (dispatch-sync [:load-page 1 (:recent-post-count settings)])
  (session/put! :current-page #'default-post-widget)) ;;page 1 amount-of-posts-per-page)

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

