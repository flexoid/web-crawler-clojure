(ns lab2.core
  (:gen-class)
  (:require [clj-http.client :as client]
            [net.cgrand.enlive-html :as html]))

;;debugging parts of expressions
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))


(def http-redirect-codes #{301 302 307})
(def http-ok 200)
(def http-not-found 404)

(declare get-page log
  process-ok process-redirect process-not-found process-unknown-error
  extract-links filter-links)

(defn crawl [url]
  (let [[status content] (get-page url)]
    (condp = status
      :ok
      (process-ok url content)

      :redirect
      (process-redirect url content)

      :not-found
      (process-not-found url)

      :unknown-error
      (process-unknown-error url content))))

(defn process-ok [url, body]
  (log url "ok")
  (let [parsed-body (html/html-resource (java.io.StringReader. body))
        links (extract-links parsed-body)]
    (doseq [link links] (crawl link))))

(defn extract-links [parsed-body]
  (let [a-tags (html/select parsed-body [:a])
        links (map #(get-in % [:attrs :href]) a-tags)
        links (filter-links links)]
    links))

(defn filter-links [links]
  (->> links
    (filter #(identity %))
    (filter #(.startsWith % "http"))))

(defn process-redirect [url, redirect-url]
  (log url "redirect")
  (crawl redirect-url))

(defn process-not-found [url]
  (log url "404"))

(defn process-unknown-error [url, status]
  (log url "unknown error"))

(defn get-page [url]
  (let [response (client/get url {:follow-redirects false :throw-exceptions false})
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


(defn log [url text]
  (println url "->" text))

(defn -main
  [url]
  (crawl url))


; (def resp (client/get "https://twitter.com" {:follow-redirects false}))

; (resp :status)
; (get-in resp [:headers :location])

; (def parsed-body (html/html-resource (java.io.StringReader. (resp :body))))
; (map (fn [link] (get-in link [:attrs :href])) (html/select parsed-body [:a]))
