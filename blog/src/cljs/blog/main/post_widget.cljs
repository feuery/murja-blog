(ns blog.main.post-widget
  (:require ;; [blog.settings :refer [settings]]
            [blog.state.post-subs]
            [re-frame.core :refer [subscribe dispatch]]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [blog.main.register-view :refer [register]]

            [blog.util :refer [in?]]
            
            blog.state.login-subs
            blog.state.editor-handlers))

(defn clean-html [html]
  (-> html
      js/String
      (.replace (js/RegExp. "<\\s*/?\\s*script\\s*>" "i") "&lt;script&gt;")
      (.replace (js/RegExp. "<\\s*/?\\s*style\\s*>" "i") "&lt;style&gt;")))

(defn comment-widget [can-edit? can-delete? {:keys [creator content created_at id]}]
  (let [{:keys [nickname img_location]} creator]
    [:div.post
     [:div.meta-actions
      (if can-edit?
        [:button "edit"])
      (if can-delete?
        [:button {:on-click #(if (js/confirm (str "Are you sure you want to delete the comment?"))
                               (dispatch [:delete-comment id]))} "delete"])]
     [:p.meta [:img.user_avatar
               {:src img_location}] "By " nickname]
     [:p.meta "Written at " (pr-str created_at)] ;; TODO add user-configurable formatting
     [:article.content {:dangerouslySetInnerHTML {:__html (clean-html content)}}]]))

(defn post-widget [{:keys [id title content creator created_at tags amount-of-comments comments next-post-id prev-post-id]} can-edit? can-delete? settings]
  (let [{:keys [nickname img_location]} creator]
    ^{:key (rand-int 9999)}
    [:div.post
     [:h2 [:a.blog-title {:href (str "/blog/post/" id)} title]]
     [:div.meta-actions
      (if can-edit?
        [:button "edit"])
      (if can-delete?
        [:button {:on-click #(if (js/confirm (str "Are you sure you want to delete post titled " title "?"))
                               (dispatch [:delete-post id]))} "delete"])]
     [:p.meta [:img.user_avatar
               {:src img_location}] "By " nickname]
     [:p.meta "Written at " (pr-str created_at)] ;; TODO add user-configurable formatting
     [:article.content {:dangerouslySetInnerHTML {:__html (if (:xss-filter-posts? settings)
                                                            (clean-html content)
                                                            content)}}]
     
     ;; TODO make a link to the post-and-its-comments view
     (if (or next-post-id prev-post-id)
       [:div
        (if prev-post-id
          [:a {:href (str "/blog/post/" prev-post-id)} "Previous post"])
        (if next-post-id
          [:a.newer-post {:href (str "/blog/post/" next-post-id)} "Next post"])])
     [:p amount-of-comments " comments"]
     (if comments
       (into [:div
              [:h1 "Comments: "]]
             (map (partial comment-widget can-edit? can-delete?) comments)))]))

;; this sets up the state
(defn default-post-widget []
  (let [page (subscribe [:current-page])
        page-nr (subscribe [:page-nr])
        user (subscribe [:current-user])
        is-empty? (subscribe [:is-empty?])
        last-page? (subscribe [:last-page?])
        settings (subscribe [:settings])]
    (fn []
      (if-not @is-empty?
        (let [{:keys [posts]} @page]
          [:div
           [:div#title-actions (if (in? (:permissions @user) "create-post")
                                 [:a {:href "/blog/create-post"}
                                  "Create post!"])]
           ;; [:div [:p (pr-str posts)]]
           (map post-widget
                 posts
                 (repeat (in? (:permissions @user) "edit-post"))
                 (repeat (in? (:permissions @user) "delete-post"))
                 (repeat @settings))
           [:div
            (if-not @last-page?
              [:a {:href (str "/blog/page/" (inc @page-nr))} "Older posts"])
            (if-not (= @page-nr 1)
              [:a.newer-post {:href (str "/blog/page/" (dec @page-nr))} "Newer posts"])
            [:p @last-page?]]])
        [register]))))
