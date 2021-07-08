(ns svg-clj.tools
  (:require [clojure.java.browse]
            [clojure.java.io]
            [hiccup.core :refer [html]]
            [svg-clj.composites :refer [svg]]
            [svg-clj.utils :as utils]))

(defn save-svg
  [svg-data fname]
  (let [data (if (= (first svg-data) :svg)
               svg-data
               (svg svg-data))]
    (spit fname (html data))))

(defn load-svg
  [fname]
  (-> fname
      slurp
      utils/svg-str->elements))

(defn cider-show
  [svg-data]
  (let [fname "_tmp.svg"]
    (save-svg svg-data fname)
    (clojure.java.io/file fname)))

(defn show
  [svg-data]
  (let [fname "_tmp.svg.html"]
    (save-svg svg-data fname)
    (clojure.java.io/file fname)))
