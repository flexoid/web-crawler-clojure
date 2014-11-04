(ns lab2.core
  (:gen-class)
  (:require [lab2.log :as log]
            [org.httpkit.client :as http]
            [net.cgrand.enlive-html :as html]
            [clojurewerkz.urly.core :as urly]))

;;debugging parts of expressions
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))


(def http-redirect-codes #{301 302 307})
(def http-ok 200)
(def http-not-found 404)

(declare get-page log
  process-ok process-redirect process-not-found process-unknown-error
  extract-links filter-links)

(defn crawl
  ([url max-depth]
    (crawl url max-depth 0))
  ([url max-depth current-depth]
    (when (<= current-depth max-depth)
        (let [[status content] (get-page url)]
          (condp = status
            :ok
            (process-ok url content max-depth current-depth)

            :redirect
            (process-redirect url content max-depth current-depth)

            :not-found
            (process-not-found url)

            :unknown-error
            (process-unknown-error url content))))))

(defn process-ok [url body max-depth current-depth]
  (log url "ok" current-depth)
  (let [parsed-body (html/html-resource (java.io.StringReader. body))
        links (extract-links parsed-body)]
    (doseq [link links] (crawl link max-depth (inc current-depth)))))

(defn extract-links [parsed-body]
  (let [a-tags (html/select parsed-body [:a])
        links (map #(get-in % [:attrs :href]) a-tags)
        links (filter-links links)]
    links))

(defn filter-links [links]
  (->> links
    (filter identity)
    (filter #(.startsWith % "http"))))

(defn process-redirect [url, redirect-url max-depth current-depth]
  (log url "redirect" current-depth)
  (crawl redirect-url max-depth (inc current-depth)))

(defn process-not-found [url]
  (log url "404"))

(defn process-unknown-error [url, status]
  (log url "unknown error"))

(defn get-page [url]
  (let [response @(http/get url {:follow-redirects false :throw-exceptions false})
        status (response :status)]
    (cond
      (= http-ok status)
      [:ok (response :body)]

      (contains? http-redirect-codes status)
      [:redirect (get-in response [:headers :location])]

      (= http-not-found status)
      [:not-found nil]

      :else
      [:unknown-error status])))


(defn log [url text & rest]
  (apply println url "->" text rest))

(defn -main
  [url max-depth]
  (crawl url (Integer/parseInt max-depth)))
