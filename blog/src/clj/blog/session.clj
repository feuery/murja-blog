(ns blog.session
  (:require [ring.util.http-response :refer [unauthorized forbidden]]
            [ring.middleware.session :refer [wrap-session]]
            [buddy.auth :refer [authenticated? throw-unauthorized]]
            [buddy.auth.backends.session :refer [session-backend]]
            [buddy.auth.accessrules :refer [wrap-access-rules]]
            [buddy.auth.middleware :refer [wrap-authentication wrap-authorization]]))

(def cookie "blog-session")

(def auth-blog (session-backend))

(defn wrap-app-session [handler]
  (-> handler
      (wrap-authorization auth-blog)
      (wrap-authentication auth-blog)
      (wrap-session {:cookie-name cookie})))

(defn access-error [rq val]
  (unauthorized val))


(defn wrap-rule [handler rule]
  (-> handler
      (wrap-access-rules {:rules [{:pattern #".*"
                                   :handler rule}]
                          :on-error access-error})))
