(ns svg-clj.tools
  (:require [clojure.java.browse]
            [clojure.java.io]
            [hiccup.core :refer [html]]
            [svg-clj.composites :refer [svg]]
            [svg-clj.elements :as el]
            [svg-clj.utils :as utils]))

(defn save-svg
  [svg-data fname]
  (let [data (if (= (first svg-data) :svg)
               svg-data
               (svg svg-data))]
    (spit fname (html data))))

(defn load-svg
  "Loads the SVG `fname` and parses the XML into a hiccup data structure, keeping all elements.
Use this function to load the SVG without throwing away any nodes, for example, if you want to keep meta and def tags."
  [fname]
  (-> fname
      slurp
      utils/svg-str->hiccup))

(defn load-svg-elems
  "Loads the SVG `fname`, parses the XML into hiccup, and returns a sequence of the SVG elements in the file.
Use this function to pull elements from an SVG that can be used directly with the other functions in this library."
  ([fname] (load-svg-elems fname el/svg-element-keys))
  ([fname key-set]
   (-> fname
       slurp
       utils/svg-str->hiccup
       (utils/get-elems key-set))))

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
