(ns murja.rss
  (:require [clojure.data.xml :as xml]
            [murja.config :refer [config]]
            [hiccup.util :refer [escape-html]])
  (:import [java.time Instant ZoneOffset ZoneId LocalDate LocalDateTime]
           [java.time.format DateTimeFormatter]))

;; (.format (.atOffset (LocalDateTime/now) (ZoneOffset/ofHours 2))  DateTimeFormatter/RFC_1123_DATE_TIME)

(defn format-datetime [dt]
  (some-> dt
          (.toInstant)
          (LocalDateTime/ofInstant (ZoneId/of "Europe/Helsinki"))))

(defn generate-link-to-post [link post-id]
  (str link "/post/" post-id))

(defn post-page->rss [series-of-post]
  (let [{:keys [title link description lang mail]} (:rss config)]
    (-> [:rss
         {:version    "2.0"
          :xmlns:atom "https://www.w3.org/2005/Atom"
          :xmlns:dc   "https://purl.org/dc/elements/1.1/"}
         [:channel
          [:title title]
          [:description description]
          [:link link]
          [:atom:link
           {:href (str link "/rss") :rel "self" :type "application/rss+xml"}]
          (map (fn [{:keys [id content created_at title]}]
                 [:item
                  [:title title]
                  [:pubDate (format-datetime created_at)]
                  [:description (escape-html (apply str (take 500 content)))]
                  [:link (generate-link-to-post link id)]])
               series-of-post)]]
        xml/sexp-as-element
        xml/emit-str)))
