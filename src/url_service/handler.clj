(ns url-service.handler
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [liberator.core :refer [resource defresource]]
            [liberator.representation :refer [render-map-generic]]
            [hiccup.core :refer [html]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [clojure.data.json :as json])
  (:import
    clojure.lang.Murmur3
    org.apache.commons.validator.routines.UrlValidator))


(def default-media-types
  ["application/json"
   "text/plain"
   "text/html"])

(defmethod render-map-generic "application/json" [data context]
  (json/write-str (conj (:links data) (:properties data))))

(defmethod render-map-generic "text/html" [data context]
  (html [:div
         [:h1 (-> data :class first)]
         [:dl
          (mapcat (fn [[key value]] [[:dt key] [:dd value]])
                  (:properties data))]]))


(def validator (UrlValidator. (into-array ["http" "https"])))

(def hash-url (comp (partial format "%x")
                    #(Murmur3/hashUnencodedChars %)))

(defrecord Urls [hashed url user-agent remote-addr referrer])

(def urls-map (atom {}))

(def activitity (atom {}))

(defn activitity-data [ctx]
  (str "URL Service Activity: <br/>" @activitity))

(defn now [] (new java.util.Date))

(defn store-activity [ctx url]
  (swap! urls-map #(assoc % (hash-url url) url))
  (let [remote-addr (str (get-in ctx [:request :remote-addr]))]
    (str remote-addr)
    (let [user-agent (str (get (get-in ctx [:request :headers]) "user-agent"))]
      (str user-agent)
      (let [refferer (str (get (get-in ctx [:request :headers]) "referrer"))]
        (swap! activitity #(assoc % (.toString (now)) (->Urls (str "http://short.ly/"  (hash-url url)) url user-agent remote-addr refferer)))))
    ))


(defresource do-hash [url]
             :allowed-methods [:options :get :post]
             :available-media-types ["text/plain" "text/html" "application/json"]
             :post! (fn [ctx]  (if (.isValid validator (str url))
                                 (store-activity ctx url)))
             :handle-created (fn [_] (if (.isValid validator (str url))
                                       (hash-url url)))
             :handle-ok (fn [_] (str "Success")))

(defresource do-unhash [shorturl]
             :allowed-methods [:options :get :post]
             :available-media-types ["text/plain" "text/html" "application/json"]
             :post! (fn [ctx] ())
             :handle-created (fn [ctx] (if (.isValid validator (str shorturl))
                                         (get @urls-map (subs shorturl (+ (.lastIndexOf shorturl "/") 1)))))
             :handle-ok (fn [ctx] (str "Success")))

(defresource get-activity
             :allowed-methods [:options :get]
             :available-media-types ["text/plain" "text/html" "application/json"]
             :handle-ok (fn [ctx]
                          (activitity-data [ctx])))


(defroutes app-routes
           (ANY "/" [] get-activity)

           (ANY "/activity" [] get-activity)

           (POST "/hash" request
             (let [params (:params request)
                   url (:url params)]
                    (do-hash url)))

           (POST "/unhash" request
             (let [params (:params request)
                   url (:url params)]
               (do-unhash url)))

           (route/resources "/")

           (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes (assoc-in site-defaults [:security :anti-forgery] false)))