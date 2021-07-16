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
            [murja.db.users :as db.users]
            [murja.config :as config]))

#_(assert (not-empty (:started (mount/start (mount/only [#'config/config #'db/db])))))
(mount/start (mount/only [#'config/config #'db/db]))

(defn get-status-and-print-if-error [{:keys [status] :as response}]
  (if-not (= (:status response) 200)
    (clojure.pprint/pprint (muuntaja/decode "application/json" (slurp (:body response)))))
  
  status)

(def decode-body (comp (partial muuntaja/decode "application/json")
                       slurp))

(deftest http-api-test
  (util/delete-test-users (:db-spec murja.db/db))
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
                                       :body decode-body)]
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
                                           :body decode-body)]
                (if-not (= status 200)
                  (clojure.pprint/pprint {:problem-body body}))
                (is (= status 200))

                (testing "| if edits are in db"
                  (println "Fetching post from url " (pr-str (str "/api/posts/post/" id)))
                  (let [{:keys [body status]} (update
                                               (app (request :get (str "/api/posts/post/" id)))
                                               :body decode-body )]
                    (is (= status 200))
                    (is (= (merge edited-post
                                  {:creator {:nickname "Test-User", :username "test-user", :img_location ""},
	                           :comments [],
	                           :amount-of-comments 0,
                                   :versions [1],
	                           :version nil} )
                           (dissoc body :created_at :next-post-id :prev-post-id)))))))))
        (testing "registering user"
          (let [{:keys [status body]} (update
                                       (app (request :get "/api/users/is-empty"))
                                       :body decode-body)]
            (is (= status 200))
            (is (false? (:is-empty? body))))
          (let [user (db.users/register-user! murja.db/db "test-user" "test-username" "" "testipassu")]
            (is (= (dissoc user :id)
                   {:username "test-username",
	            :nickname "test-user",
	            :img_location ""}))

            ;; this is untestable because I don't have the slightest clue how I should mock session here
            #_(testing "updating user"
              (binding [murja.middleware/*test-user* user]
                (let [{:keys [status body]} (update
                                             (app (-> (request :post "/api/users/save")
                                                      (json-body (-> user
                                                                     (assoc :username "test-updated-username"
                                                                            :nickname "test-PÄIVITETTY NIKKI"
                                                                            :password "testipassu")))))
                                             :body decode-body)]
                  (is (= status 200))
                  (is (empty? body)))))))
        (testing "/api/posts/all-titles shows hidden posts too"
          (testing "let's insert a hidden post there"
            (let [{:keys [status body]} (update
                                         (app (-> (request :post "/api/posts/post")
                                                  (json-body {:title "hidden title"
                                                              :content "Testi contenttia"
                                                              :tags ["hidden" "test-generated"]})))
                                         :body decode-body)]
              (is (= status 200))))
          (let [{:keys [status body] :as result} (update (app (request :get "/api/posts/all-titles")) :body decode-body)]
            (is ((set (mapcat :Tags body)) "hidden")))))))

          
  (util/delete-test-users (:db-spec murja.db/db))
  (util/delete-test-posts (:db-spec murja.db/db)))
