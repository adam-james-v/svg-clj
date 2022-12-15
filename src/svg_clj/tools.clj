(ns svg-clj.tools
  (:require [clojure.java.browse]
            [clojure.java.io]
            [hiccup.core :as hiccup :refer [html]]
            [svg-clj.composites :as comp :refer [svg]]
            [svg-clj.utils :as u]))

(defn save-svg
  "Save hiccup-style `svg-data` to file `fname`."
  [svg-data fname]
  (let [data (if (= (first svg-data) :svg)
               svg-data
               (svg svg-data))]
    (spit fname (html data))))

(defn cider-show
  "Show hiccup-style `svg-data` in a CIDER REPL. Creates `_tmp.svg` in the project's root folder."
  [svg-data]
  (let [fname "_tmp.svg"]
    (save-svg svg-data fname)
    (clojure.java.io/file fname)))

(defn show
  "Show hiccup-style `svg-data` in the browser. Creates `_tmp.svg.html` in the project's root folder."
  [svg-data]
  (let [fname "_tmp.svg.html"]
    (save-svg svg-data fname)
    (clojure.java.io/file fname)))
