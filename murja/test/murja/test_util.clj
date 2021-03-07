(ns murja.test-util
  (:require [hugsql.core :refer [def-db-fns]]))

(def-db-fns "test_util.sql")

(defn test-user-really-exists? [{:keys [db-spec]}]
  (:exists (test-user-exists? db-spec)))

(def test-admin (atom {:username "test-user", :password "no this doesn't work" :nickname "Test-User", :img_location "", :primary-group-name "Admins", :primarygroup true, :userid 1, :permissions ["create-page" "create-post" "create-comment" "delete-post" "edit-post" "comment-post" "edit-comment" "delete-comment" "delete-user" "edit-user" "edit-self" "can-import"]}))

(defn init-users [{:keys [db-spec] :as db}]
  (when-not (test-user-really-exists? db)
    (insert-test-user! db-spec @test-admin))
  (swap! test-admin assoc :_id (:id (test-user-id db-spec))))

  

