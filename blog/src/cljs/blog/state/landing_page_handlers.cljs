(ns blog.state.landing-page-handlers
  (:require [re-frame.core :refer [dispatch reg-event-db reg-event-fx trim-v]]))

(reg-event-fx :query-landing-page
              [trim-v]
              (fn [{:keys [db]} _]
                {:db db
                 :get {:url "/api/posts/existing-landing-page"
                       :dispatch-key :landing-page-result}}))

(reg-event-fx :landing-page-result
              [trim-v]
              (fn [{:keys [db]} [{:keys [id] :as result}]]
                (js/console.log (pr-str result))
                {:db db
                 :redirect-to (if (empty? result)
                                (str "/blog/page/1")
                                (str "/blog/post/" id))}))
