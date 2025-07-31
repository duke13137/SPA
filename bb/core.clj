#!/usr/bin/env bb
(ns core 
  (:require 
    [clojure.string :as str]
    [cheshire.core :as json]
    [hiccup2.core :as h]
    [org.httpkit.server :as srv]
    [ring.middleware.params :refer [wrap-params]]
    [ruuter.core :as ruuter]
    [selmer.parser :refer [render-file]]
    [starfederation.datastar.clojure.api :as d*]
    [starfederation.datastar.clojure.adapter.http-kit2 :as hk]))

(require '[playback.core])
(require '[sc.api :as sc])
(require '[portal.console :as log])

(comment
  (require '[promesa.core :as p]
           '[promesa.exec.csp :as sp :refer [go >! <!]])
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
       (fn [_sse status]
         (println status))
       hk/on-exception
       (fn [e]
         (println e))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn hello-page [req]
  {:status 200
   :body (render-file "hello-world.html" {})})

(defn index [req]
  {:status 200
   :body (render-file "index.html" {})})

(def message "Hello, world!")

(defn hello-sse [req sse]
  (log/info req)
  (let [d (-> req get-signals :delay int)]
    (dotimes [i (count message)]
      (d*/patch-elements! sse
        (html [:h1 {:id "message"}
               (subs message 0 (inc i))]))
      (Thread/sleep d))))
    ;; (d*/execute-script! sse "alert('DONE')")))

(import '[java.time LocalTime]
        '[java.time.format DateTimeFormatter])

(def dt-format (DateTimeFormatter/ofPattern "HH:mm:ss"))

(defn now []
  (LocalTime/.format (LocalTime/now) dt-format))

(defn clock-sse [req sse]
  (sc/spy)
  (d*/patch-elements! sse
    (html [:h1 {:id "clock"
                :data-on-interval__duration.1s (d*/sse-get "/clock-sse")}
           (now)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routes
;;;;;;;;;;;;;;;;;;;;;;;;;;

(def routes [{:path     "/"
              :method   :get
              :response index}
             {:path     "/hello"
              :method   :get
              :response hello-page}
             {:path     "/hello-sse"
              :method   :get
              :response (sse #'hello-sse)}
             {:path     "/clock-sse"
              :method   :get
              :response (sse #'clock-sse)}])

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main [& args]
  (let [url (str "http://localhost:" port "/")]
    (srv/run-server (hk/wrap-start-responding
                      (wrap-params
                        #(ruuter/route routes %)))
                    {:port port})
    (println "serving" url)))

(when (= *file* (System/getProperty "babashka.file"))
  (-main nil))
