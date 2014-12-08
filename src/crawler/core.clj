(ns crawler.core
  (:gen-class)
  (:require [crawler.logger :as logger]
            [org.httpkit.client :as http]
            [net.cgrand.enlive-html :as html]
            [clojurewerkz.urly.core :as urly]))

;;debugging parts of expressions
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))


(def http-redirect-codes #{301 302 307})
(def http-ok 200)
(def http-not-found 404)

(def printer (agent nil))

(declare get-page print-log print-progress add-log
  process-ok process-redirect process-not-found process-unknown-error
  extract-links filter-links)

(defn crawl
  ([log url max-depth]
    (crawl log url max-depth 1))
  ([log url max-depth current-depth]
    (when (<= current-depth max-depth)
        (let [[status content] (get-page url)]
          (print-progress)
          (condp = status
            :ok
            (let [[log links] (process-ok log url content max-depth current-depth)]
              (doall
                (pmap
                  (fn [link]
                    (crawl log link max-depth (inc current-depth)))
                  links)))

            :redirect
            (let [redirect-url content
                  [log url] (process-redirect log url redirect-url)]
              (if url
                (recur log redirect-url max-depth (inc current-depth))))

            :not-found
            (process-not-found log url)

            :unknown-error
            (process-unknown-error log url content))))))

(defn process-ok [log url body max-depth current-depth]
  (try
    (let [parsed-body (html/html-resource (java.io.StringReader. body))
          links (extract-links parsed-body)
          links (filter-links links)
          log (add-log log url "OK " (count links) " links")]
      ; (print-log url " OK")
      [log links])
    (catch java.lang.ClassCastException e [log []])))


(defn extract-links [parsed-body]
  (let [a-tags (html/select parsed-body [:a])
        links (map #(get-in % [:attrs :href]) a-tags)]
    links))

(defn filter-links [links]
  (->> links
    (filter identity)
    (filter #(.startsWith % "http"))))

(defn process-redirect [log url redirect-url]
  (let [redirect-url (first (filter-links [redirect-url]))]
    (if redirect-url
      [(add-log log url "REDIRECT" " " redirect-url) url])))

(defn process-not-found [log url]
  (add-log log url "404"))

(defn process-unknown-error [log url status]
  (add-log log url "UNKNOWN ERROR"))

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

(defn print-progress []
  (send printer
    (fn [_]
      (print ".")
      (flush)
      :ok)))

(defn print-log [url text & rest]
  (apply println url " -> " text rest))

(defn add-log [log url & rest]
  (logger/add-record (apply str url " -> " rest) log))

(defn read-urls-from-file [path]
  {:pre [(not (nil? path))]}
  (with-open [r (clojure.java.io/reader path)]
    (doall (line-seq r))))

(defn -main
  [file max-depth]
  (let [crawl-log (logger/init-log)
        urls (read-urls-from-file file)
        max-depth (Integer/parseInt max-depth)]
    (doall (pmap (fn [url]
      (crawl crawl-log url max-depth)) urls))
    (logger/print-tree crawl-log)
    (shutdown-agents)))
