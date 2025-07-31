#!/usr/bin/env bb

(ns spa)

(require '[org.httpkit.server :as srv]
         '[ring.middleware.params :refer [wrap-params]]
         '[ruuter.core :as ruuter]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[hiccup2.core :as h]
         '[selmer.parser :refer [render-file]])

(require '[starfederation.datastar.clojure.api :as d*]
         '[starfederation.datastar.clojure.adapter.http-kit :as hk])

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def port 3000)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-json [s]
  (json/parse-string s true))

(defn get-signals [req]
  (-> req d*/get-signals parse-json))

(defn html [h]
  (str (h/html h)))

(defn sse [handler]
  (fn [req]
    (hk/->sse-response req
      {hk/on-open
       (fn [sse]
         (d*/with-open-sse sse
           (handler req sse)))
       hk/on-close
       (fn [_sse status]
         (println status))
       hk/on-exception
       (fn [e]
         (println e))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn index [req]
  {:status 200
   :body (render-file "hello-world.html" {})})

(def message "Hello, world!")

(defn hello [req sse]
  (let [d (-> req get-signals :delay int)]
    (dotimes [i (count message)]
      (d*/patch-elements! sse
        (html [:div {:id "message"}
               (subs message 0 (inc i))]))
      (Thread/sleep d))
    (d*/execute-script! sse "alert('DONE')")))

(import 'java.time.format.DateTimeFormatter
        'java.time.LocalDateTime)

(def dt-format (DateTimeFormatter/ofPattern "HH:mm:ss"))

(defn now []
  (LocalDateTime/.format (LocalDateTime/now) dt-format))

(defn clock [req sse]
  (d*/patch-elements! sse
    (html [:div {:id "clock"
                 :data-on-interval__duration.1s (d*/sse-get "/clock")}
           (now)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routes
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def routes [{:path     "/"
              :method   :get
              :response index}
             {:path     "/hello"
              :method   :get
              :response (sse hello)}
             {:path     "/clock"
              :method   :get
              :response (sse clock)}])

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main [& args]
  (let [url (str "http://localhost:" port "/")]
    (srv/run-server (wrap-params #(ruuter/route routes %)) {:port port})
    (println "serving" url)
    @(promise)))

(when (= *file* (System/getProperty "babashka.file"))
  (-main nil))
