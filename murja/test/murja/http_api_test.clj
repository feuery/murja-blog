(ns murja.http-api-test
  (:require [clojure.test :refer :all]
            [mount.core :as mount]
            [murja.test-util :as util]
            [murja.db.login :refer [can?]]
            [murja.middleware :as middleware]
            [muuntaja.core :as muuntaja]
            [ring.mock.request :refer [request json-body]]
            [murja.reitit :as reitit]
            [murja.db :as db]
            [murja.config :as config]))

#_(assert (not-empty (:started (mount/start (mount/only [#'config/config #'db/db])))))
(mount/start (mount/only [#'config/config #'db/db]))

(defn get-status-and-print-if-error [{:keys [status] :as response}]
  (if-not (= (:status response) 200)
    (clojure.pprint/pprint (muuntaja/decode "application/json" (slurp (:body response)))))
  
  status)

(deftest http-api-test
  (util/init-users murja.db/db)
  (testing "| if stuff that's supposed to return something from test db instead of spec errors does that "
    (with-redefs [middleware/wrapping-user (fn [handler req]
                                             (handler (assoc req :user @util/test-admin)))
                  can? (constantly true)]
      

      (let [app (reitit/app {})]
        (testing "| /api/posts/titles"
          (let [result (app (request :get "/api/posts/titles"))
                status (get-status-and-print-if-error result)]       
            (is (= (:status result) 200))))
        (testing "| POST /api/posts/post"
          (let [{:keys [status body]} (update
                                       (app (-> (request :post "/api/posts/post")
                                                (json-body {:title "Uusi testi title"
                                                            :content "Testi contenttia"
                                                            :tags [#_"hidden" "test-generated"]})))
                                       :body (comp (partial muuntaja/decode "application/json")
                                                   slurp))]
            (is (= status 200))

            (testing "| PUT /api/posts/post"
              (let [{:keys [id]} body
                    edited-post {:title "Muokattu title"
                                 :id id
                                 :content "Muokattua contenttia"
                                 :tags ["test-generated" "edited-tag"]}
                    {:keys [body status]} (update
                                           (app (-> (request :put "/api/posts/post")
                                                    (json-body edited-post)))
                                           :body (comp (partial muuntaja/decode "application/json")
                                                       slurp))]
                (if-not (= status 200)
                  (clojure.pprint/pprint {:problem-body body}))
                (is (= status 200))

                (testing "| if edits are in db"
                  (println "Fetching post from url " (pr-str (str "/api/posts/post/" id)))
                  (let [{:keys [body status]} (update
                                               (app (request :get (str "/api/posts/post/" id)))
                                               :body (comp (partial muuntaja/decode "application/json")
                                                           slurp))]
                    (is (= status 200))
                    (is (= (merge edited-post
                                  {:creator {:nickname "Test-User", :username "test-user", :img_location ""},
	                           :comments [],
	                           :amount-of-comments 0,
                                   :versions [1],
	                           :version nil} )
                           (dissoc body :created_at :next-post-id :prev-post-id))))))))))))
  (util/delete-test-posts (:db-spec murja.db/db)))
