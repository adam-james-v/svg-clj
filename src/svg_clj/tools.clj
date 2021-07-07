(ns svg-clj.tools
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [clojure.java.browse]
            [clojure.java.io]
            [hiccup.core :refer [html]]
            [svg-clj.elements :as svg]
            [svg-clj.composites :refer [svg]]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
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
    (do (save-svg svg-data fname)
        (clojure.java.io/file fname))))

(defn show
  [svg-data]
  (let [fname "_tmp.svg.html"]
    (do (save-svg svg-data fname))
        (clojure.java.io/file fname)))
