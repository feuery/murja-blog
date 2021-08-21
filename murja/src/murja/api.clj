(ns murja.api
  (:require [murja.config :as config]
            [hiccup.page :refer [include-js include-css html5]]
            [clojure.java.io :as io]
            [murja.api.posts :as api.posts]
            [murja.api.users :as api.users]
            [murja.api.login :as api.login]
            [murja.api.media :as api.media]
            [murja.db.posts :as post-db]))

(defn get-client-settings [_]
  {:status 200
   :body (:client-config config/config)})

(defn get-users-is-empty [{:keys [db]}]
  {:pre [(some? db)]}
  {:status 200
   :content-type "application/transit+json"
   :body {:is-empty? (api.users/is-empty db)}})

(defn post-users-save [{:keys [db session]
                        {new-user :body} :parameters}]
  (api.users/save-user db (merge (:identity session)
                                 new-user))
  {:status 200
   :body (dissoc new-user :password)})

(defn post-login [{:keys [db session]
                   {login-user :body} :parameters}]
  (if-let [login-data (api.login/do-login db login-user)]
    {:status 200
     :body (dissoc login-data :userid)
     :session (assoc session :identity {:_id (:userid login-data)
                                        :groups (api.login/user-groups db login-user)}) }
    {:status 401
     :body "Unauthorized"}))

(defn post-logout [_]
  {:status 204
   :session nil})

(defn get-session [{:keys [db]
                    {:keys [_id]} :user}]
  (assert (some? _id))
  {:status 200
   :body (api.login/get-user-view-data db _id)})

(defn get-posts-titles [{:keys [db]}]
  {:status 200
   :body (api.posts/get-titles-by-year db :show-hidden? false)})

(defn get-posts-all-titles [{:keys [db]}]
  {:status 200
   :body (api.posts/get-titles-by-year db :show-hidden? true)})


(defn get-existing-landing-page [{:keys [db]}]
  {:status 200
   :body (api.posts/get-existing-landing-page db)})

(defn get-post-version [{:keys [db parameters]}]
  (let [{{post-id :id
         post-version :version} :path} parameters]

    {:status 200
     :body (api.posts/get-post-version-by-id db post-id post-version)}))

(defn get-post-id [{:keys [db parameters]}]
  (let [{{post-id :id} :path} parameters]

    {:status 200
     :body (api.posts/get-post-by-id db post-id :show-hidden? false)}))

(defn get-post-id-allow-hidden [{:keys [db parameters]}]
  (let [{{post-id :id} :path} parameters]
    {:status 200
     :body (api.posts/get-post-by-id db post-id :show-hidden? true)}))

(defn get-id-versions [{:keys [db parameters]}]
  (let [{{post-id :id} :path} parameters]
    {:status 200
     :body (api.posts/get-post-versions db post-id)}))

(defn delete-post-version [{:keys [db parameters]}]
  (let [{{post-id :id
          post-version :version} :path} parameters]
    {:status 200
     :body (api.posts/delete-post-version db post-id post-version)}))

(defn delete-post-id [{:keys [db parameters]}]
  (let [{{post-id :id} :path} parameters]
    (api.posts/delete-post-id db post-id)
    {:status 200
     :body {:post-id post-id}}))

(defn delete-comment-id [{:keys [db parameters]}]
  (let [{{comment-id :id} :path} parameters]
    (api.posts/delete-comment-id db comment-id)
    {:status 200
     :body {:comment-id comment-id}}))

(defn get-all-limit [{:keys [db parameters]}]
  (let [{{:keys [limit]} :path} parameters]
    {:status 200
     :body (api.posts/get-all db limit)}))

(defn get-page [{:keys [db parameters]}]
  (let [{{:keys [page page-size]} :path} parameters]
    {:status 200
     :body (api.posts/get-page db page page-size)}))


(defn create-post [{:keys [db parameters user]}]
  (let [{new-post :body} parameters]
    {:status 200
     ;; we have to return a body to not confuse json parser in the emacs end
     :body (api.posts/create-post db user new-post)}))

(defn edit-post [{:keys [db user parameters]}]
  (let [{edited-post :body} parameters]
    (api.posts/edit-post db user edited-post)
    {:status 200
     ;; we have to return a body to not confuse json parser in the emacs end
     :body {:success? true}}))

(defn comment-post [{:keys [db user parameters]}]
  (let [{new-comment :body} parameters]
    (api.posts/comment-post db user new-comment)
    {:status 204}))

(defn get-site.css [_]
  {:status 200
   :body (slurp (io/resource "public/css/site.css"))})

(defn get-path [rq]
  {:post [(some? %)]}
  (get-in rq [:reitit.core/match :path]))

(defn post-pictures [{:keys [db]
                      {{:keys [file]} :multipart} :parameters}]
  {:status 200
   :body (api.media/save-image db file)})

(defn get-pictures [{:keys [db]
                     {{:keys [id]} :path} :parameters}]
  (let [{:keys [data name]} (api.media/get-picture db id)]
    {:status 200
     :body data
     :headers {"Content-Disposition" (str "inline; filename=" name)}}))

(defn get-pictures-list [{:keys [db]}]
  {:status 200
   :body (api.media/list-pictures db)})






  

(defn get-frontend [{:keys [db] :as rq}]
  (let [{:keys [js-route css-route]} config/config
        path (get-path rq)
        post-meta (if-let [[_ id] (re-matches #"/blog/post/(\d+)" path)]
                    (post-db/make-fb-meta-tags db id))
        js (if (not-empty js-route)
             (slurp js-route)
             (slurp (io/resource "murja.min.js")))]

    {:status 200
     :headers {"content-type" "text/html"}
     :body (html5 {:xmlns:og "http://ogp.me/ns#"
                   :xmlns:fb "http://www.facebook.com/2008/fbml"}
                  (into [:head
                         (include-css css-route)
                         [:script {:src "https://pagecdn.io/lib/ace/1.4.12/ace.js"}]
                         [:meta {:charset "UTF-8"}]                         
                         [:script js]]
                        post-meta)
                  [:body
                   [:div#app]
                   [:script
                    "var app = Elm.Main.init({
        node: document.getElementById(\"app\")
    });
app.ports.alert.subscribe( (prompt) => {
  window.alert(prompt);
});

app.ports.runAce.subscribe( (content) => {

  let editor = ace.edit(\"editor-post-content\");
  editor.setTheme(\"ace/theme/monokai\");
  editor.session.setMode('ace/mode/html');
  editor.setKeyboardHandler(\"ace/keyboard/emacs\");
  editor.session.setValue(content);
  
  editor.on('change', event => {
    let value = editor.getSession().getValue();
    app.ports.aceStateUpdate.send(value);
  });

  const config = { attributes: false, childList: true};
  const callback = function(mutationsList, observer) {
    editor.destroy();
    editor = null;
  };
  const observer = new MutationObserver(callback);
  observer.observe(document.getElementById('editor-post-content'), config);

  console.log(\"ace should be initiated\");
});

app.ports.prompt.subscribe( (prompt) => {
  let value = window.prompt(prompt);
  app.ports.tags.send(value);
});
"]])}))
