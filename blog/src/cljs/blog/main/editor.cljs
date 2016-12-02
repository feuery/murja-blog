(ns blog.main.editor
  (:require [re-frame.core :refer [dispatch subscribe]]
            [clojure.string :as str]
            blog.state.editor-handlers

            [blog.util :refer [value-of ctrl-clicked?
                               only-ctrl-clicked?]]))

(defn emacs-ctrl-keychord? [e]
  (and (ctrl-clicked? e)
       (not (only-ctrl-clicked? e))))

(defn emacs-chord [e]
  (str (if (.-ctrlKey e) "C-" "")
       (if (.-metaKey e) "M-" "")
       (if (.-shifKey e) "S-" "")
       (.-key e)))

(defn post-editor [post]
  [:div#container
   [:div#page.editor-container
    [:div#title-actions
     [:button {:on-click #(if (js/confirm "Do you really want to publish this post? At the moment it can't be undone without SQL magic")
                                            ;; The post is being edited in the re-frame's store so no need to pass it as a param here
                                            (dispatch [:publish-post]))} "Publish post!"]]
    [:label "Title"]
    [:input {:type :text
             :on-change #(dispatch [:set-post-title (value-of %)])
             :value (:title post)}]
    [:label "Content"]
    [:textarea#content-editor {:on-change #(dispatch [:set-post-content (value-of %)])
                :on-keyDown #(when (emacs-ctrl-keychord? %)
                               (println (emacs-chord %))
                               (.preventDefault %))
                :value (:content post)}]]
   [:div#sidebar
    [:label "Tags "]
    [:input {:type :text
             :value (:tags post)
             :on-change #(dispatch [:set-post-tags (value-of %)])}]
    [:button {:on-click #(dispatch [:set-editor-visibility false])}
     "Hide editor"]]])

(defn editor-container [visible?]
  (let [post (subscribe [:edited-post])]
    (fn [visible?]
      (if visible?
        [post-editor @post]))))
