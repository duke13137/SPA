(ns todoapp ;; Datastar Todo App
  (:require
   [clojure.string :as str]
   [hiccup2.core :as h]
   [cheshire.core :as json]
   [selmer.parser :refer [render-file]]
   [starfederation.datastar.clojure.api :as d*]
   [starfederation.datastar.clojure.adapter.http-kit2 :as hk]))

(require '[babashka.pods :as pods])
(pods/load-pod 'huahaiy/datalevin "0.10.5")
(require '[pod.huahaiy.datalevin :as d])

(require '[sc.api :as sc])

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datalevin
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema
  {:todo/name {:db/valueType :db.type/string}
   :todo/done {:db/valueType :db.type/boolean}})

(def conn (d/get-conn "/tmp/bb-todos" schema))

(defn ->todo
  "Convert Datalevin pull result to component-friendly map."
  [m]
  {:id (:db/id m) :name (:todo/name m) :done (boolean (:todo/done m))})

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CRUD
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-todo! [name]
  (d/transact! conn [{:todo/name name :todo/done false}]))

(defn toggle-todo! [id]
  (let [done (:todo/done (d/pull (d/db conn) [:todo/done] id))]
    (d/transact! conn [[:db/add id :todo/done (not done)]])))

(defn update-todo-name! [id name]
  (d/transact! conn [[:db/add id :todo/name name]]))

(defn remove-todo! [id]
  (d/transact! conn [[:db/retractEntity id]]))

(defn remove-all-completed! []
  (let [ids (d/q '[:find [?e ...] :where [?e :todo/name _]] (d/db conn))
        all (map #(d/pull (d/db conn) [:db/id :todo/done] %) ids)
        completed (filter :todo/done all)]
    (when (seq completed)
      (d/transact! conn (mapv #(vector :db/retractEntity (:db/id %)) completed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Queries
;; NOTE: Boolean Datalog queries are broken in Datalevin pod
;; (e.g. [?e :todo/done false] matches true). Use pull + Clojure filter.
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-all-todos []
  (let [ids (d/q '[:find [?e ...] :where [?e :todo/name _]] (d/db conn))]
    (->> ids
         (map #(->todo (d/pull (d/db conn) [:db/id :todo/name :todo/done] %)))
         (sort-by :id))))

(defn filtered-todos [filter-name]
  (let [all (get-all-todos)]
    (case filter-name
      "active"    (remove :done all)
      "completed" (filter :done all)
      all)))

(defn get-todo [id]
  (->todo (d/pull (d/db conn) [:db/id :todo/name :todo/done] id)))

(defn get-items-left []
  (count (remove :done (get-all-todos))))

(defn todos-completed []
  (count (filter :done (get-all-todos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn html [hiccup]
  (str (h/html hiccup)))

(defn get-signals [req]
  (let [raw (d*/get-signals req)]
    (json/parse-string (if (string? raw) raw (slurp raw)) true)))

(defn path-id [req]
  (parse-long (first (:path-params req))))

(def sse-connections (atom #{}))

(defn use-sse [handler]
  (fn [req]
    (hk/->sse-response req
      {hk/on-open
       (fn [sse]
         (swap! sse-connections conj sse)
         (d*/with-open-sse sse
           (handler req sse)))
       hk/on-close
       (fn [sse status]
         (swap! sse-connections disj sse)
         (println status))
       hk/on-exception
       (fn [e]
         (println e))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Components
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn todo-item [{:keys [id name done]}]
  [:li {:id (str "todo-" id)
        :class (when done "completed")}
   [:div.view
    [:input.toggle {:type "checkbox"
                    :checked done
                    :data-on:click (str "@patch('/todos/sse/done/" id "')")}]
    [:label {:data-on:dblclick (str "@get('/todos/sse/edit/" id "')")}
     name]
    [:button.destroy
     {:data-on:click (str "@delete('/todos/sse/" id "')")}]]])

(defn todo-edit-form [id name]
  [:li {:id (str "todo-" id) :class "editing"}
   [:input.edit {:data-bind:edittext ""
                 :data-on:keydown (str "evt.key === 'Enter' && @patch('/todos/sse/name/" id "')")
                 :data-on:blur (str "@patch('/todos/sse/name/" id "')")
                 :autofocus true}]])

(defn todo-list [todos]
  (map todo-item todos))

(defn item-count []
  (let [n (get-items-left)]
    [:span#todo-count.todo-count
     [:strong n] (if (= 1 n) " item left" " items left")]))

(defn clear-completed-button []
  [:button#clear-completed.clear-completed
   {:data-on:click "@delete('/todos/sse/clear')"
    :class (when-not (pos? (todos-completed)) "hidden")}
   "Clear completed"])

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Patch helper
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn patch-all! [sse filter-name]
  (let [todos (filtered-todos (or filter-name "all"))]
    (d*/patch-elements! sse (html [:ul#todo-list.todo-list (todo-list todos)]))
    (d*/patch-elements! sse (html (item-count)))
    (d*/patch-elements! sse (html (clear-completed-button)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SSE Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn list-todo [req sse]
  (let [{:keys [filter]} (get-signals req)]
    (patch-all! sse filter)
    (d*/close-sse! sse)))

(defn add-todo [req sse]
  (sc.api/spy)
  (let [{:keys [todo filter]} (get-signals req)]
    (when-not (str/blank? todo)
      (add-todo! todo))
    (patch-all! sse filter)
    (d*/patch-signals! sse (json/generate-string {:todo ""}))
    (d*/close-sse! sse)))

(defn edit-todo [req sse]
  (let [id (path-id req)
        todo (get-todo id)]
    (d*/patch-signals! sse (json/generate-string {:edittext (:name todo)}))
    (d*/patch-elements! sse (html (todo-edit-form id (:name todo))))
    (d*/close-sse! sse)))

(defn save-todo [req sse]
  (let [{:keys [edittext filter]} (get-signals req)
        id (path-id req)]
    (when-not (str/blank? edittext)
      (update-todo-name! id edittext))
    (patch-all! sse filter)
    (d*/close-sse! sse)))

(defn toggle-todo [req sse]
  (let [{:keys [filter]} (get-signals req)
        id (path-id req)]
    (toggle-todo! id)
    (patch-all! sse filter)
    (d*/close-sse! sse)))

(defn delete-todo [req sse]
  (let [{:keys [filter]} (get-signals req)
        id (path-id req)]
    (remove-todo! id)
    (patch-all! sse filter)
    (d*/close-sse! sse)))

(defn clear-todo [req sse]
  (let [{:keys [filter]} (get-signals req)]
    (remove-all-completed!)
    (patch-all! sse filter)
    (d*/close-sse! sse)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page handler
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn app-index [req]
  (let [todos (get-all-todos)]
    {:status 200
     :body (render-file "todo.html"
                        {:initial-todos (html (todo-list todos))
                         :item-count (html (item-count))
                         :clear-completed (html (clear-completed-button))})}))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routes
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def routes
  {"GET /todos"               app-index
   "GET /todos/sse"           (use-sse #'list-todo)
   "POST /todos/sse"          (use-sse #'add-todo)
   "GET /todos/sse/edit/*"    (use-sse #'edit-todo)
   "PATCH /todos/sse/name/*"  (use-sse #'save-todo)
   "PATCH /todos/sse/done/*"  (use-sse #'toggle-todo)
   "DELETE /todos/sse/clear"  (use-sse #'clear-todo)
   "DELETE /todos/sse/*"      (use-sse #'delete-todo)})
