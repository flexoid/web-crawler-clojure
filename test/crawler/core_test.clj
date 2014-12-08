(ns crawler.core-test
  (:require [clojure.test :refer :all]
            [crawler.core :refer :all]
            [net.cgrand.enlive-html :as html]
            [org.httpkit.client :as http]))

(deftest test-extract-links
  (testing "Extracts all links from html"
    (let [html-string "<a href=\"http://example.com/link\">Link</a>
            <h1>some</h1><a href=\"/test/link2\">Link</a>"
          html-fragment (html/html-resource (java.io.StringReader. html-string))]
      (is (=
        (extract-links html-fragment)
        ["http://example.com/link", "/test/link2"])))))

(deftest test-filter-links
  (testing "Leaves only absolute links"
    (is (=
      (filter-links ["http://example.com/index", "relative.html", "/another.html", "https://secure.com/test"])
      ["http://example.com/index", "https://secure.com/test"]))))

(deftest test-get-page
  (testing "Handles 404 status"
    (with-redefs-fn {#'http/get (fn [& _] (atom {:status 404}))}
      #(is (=
        (get-page "URL")
        [:not-found nil]))))

  (testing "Handles 500 status as unknown error"
    (with-redefs-fn {#'http/get (fn [& _] (atom {:status 500}))}
      #(is (=
        (get-page "URL")
        [:unknown-error 500])))))

