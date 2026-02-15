#!/usr/bin/env bb
(ns core 
  (:require 
    [clojure.string :as str]
    [cheshire.core :as json]
    [hiccup2.core :as h]
    [org.httpkit.server]
    [ring.middleware.params :refer [wrap-params]]
    [clj-simple-router.core :as router]
    [promesa.core :as p]
    [promesa.exec.csp :as sp :refer [go >! <!]]
    [selmer.parser :refer [render-file]]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.http-kit2 :as hk]))

(require '[playback.core])
(require '[portal.console :as log])
(require '[sc.api :as sc])
(require '[sci.nrepl.browser-server :as browser])
(require '[babashka.nrepl.server :as nrepl])

(comment

  'comment)

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
       (fn [sse status]
         (println status))
       hk/on-exception
       (fn [e]
         (println e))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn index [req]
  {:status 200
   :body (render-file "index.html" {})})

(defn js [req]
  {:status 200
   :body (render-file "client.cljs" {})})

(def message "Hello from server!")

(defn hello-sse [req sse]
  (log/info req)
  (let [d (-> req get-signals :delay int)]
    (dotimes [i (count message)]
      (d*/patch-elements! sse
        (html [:h1 {:id "message"}
               (subs message 0 (inc i))]))
      (Thread/sleep d))
    (d*/console-log! sse message)))

(import '[java.time LocalTime]
        '[java.time.format DateTimeFormatter])

(def dt-format (DateTimeFormatter/ofPattern "HH:mm:ss"))

(defn now []
  (.format (LocalTime/now) dt-format))

(defn clock-sse [req sse]
  (d*/patch-elements! sse
    (html [:h1 {:id "clock"
                :data-on-interval__duration.1s (d*/sse-get "/clock-sse")}
           (now)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routes
;;;;;;;;;;;;;;;;;;;;;;;;;;
(def routes {"GET /" index
             "GET /client.cljs" js
             "GET /hello-sse" (sse #'hello-sse)
             "GET /clock-sse" (sse #'clock-sse)})

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main [& args]
  (let [url (str "http://localhost:" port "/")]
    (def srv (org.httpkit.server/run-server
              (-> (router/router routes)
                  (wrap-params)
                  (hk/wrap-start-responding))
              {:port port}))
    (println "serving" url)))

(when (= *file* (System/getProperty "babashka.file"))
  (browser/start! {:nrepl-port 1333 :websocket-port 1340})
  (nrepl/start-server! {:host "127.0.0.1" :port 1666})
  (-main)
  #_(srv)
  @(promise))
