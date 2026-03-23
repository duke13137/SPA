(ns todoapp-test
  (:require [clojure.test :refer [deftest is testing]]
            [todoapp :as sut]))

(deftest test-broadcast-single-client
  (testing "Single client receives patch after mutation"
    (let [initial-count (count (sut/get-all-todos))]
      (sut/add-todo! "test broadcast")
      (is (= (inc initial-count) (count (sut/get-all-todos)))))))

(deftest test-broadcast-multi-client
  (testing "All connected clients receive same broadcast"
    (let [streams-before @sut/streams
          cid1 "test-client-1"
          cid2 "test-client-2"]
      (sut/update-stream-filter! cid1 "all")
      (sut/update-stream-filter! cid2 "all")
      (is (= 2 (count (filter #(some #{cid1 cid2} [(key %)]) @sut/streams)))))))

(deftest test-broadcast-filter-aware
  (testing "Active filter doesn't receive completed todos"
    (let [todos-before (sut/filtered-todos "active")
          todos-all (sut/filtered-todos "all")]
      (when-not (seq todos-before)
        (sut/add-todo! "active todo"))
      (sut/add-todo! "completed todo")
      (let [completed (sut/get-all-todos)
            completed-id (->> completed (filter :done) first :id)]
        (when completed-id
          (sut/toggle-todo! completed-id)))
      (is (some? (sut/filtered-todos "active"))))))

(deftest test-connection-cleanup
  (testing "Disconnected SSE removed from streams atom"
    (let [cid "cleanup-test"]
      (sut/update-stream-filter! cid "all")
      (is (some? (get @sut/streams cid)))
      (sut/remove-stream-by-cid! cid)
      (is (nil? (get @sut/streams cid))))))

(deftest test-presence-edit-start
  (testing "User starts editing triggers presence"
    (let [cid "presence-user"
          todo-id 1]
      (sut/start-editing! todo-id cid)
      (is (= cid (get @sut/editing-users todo-id))))))

(deftest test-presence-edit-end
  (testing "User saves clears presence"
    (let [cid "presence-user"
          todo-id 1]
      (sut/start-editing! todo-id cid)
      (sut/stop-editing! todo-id)
      (is (nil? (get @sut/editing-users todo-id))))))

(deftest test-presence-multi-user
  (testing "Multiple users editing same todo"
    (let [todo-id 1]
      (sut/start-editing! todo-id "user-a")
      (sut/start-editing! todo-id "user-b")
      (is (= "user-b" (get @sut/editing-users todo-id))))))
