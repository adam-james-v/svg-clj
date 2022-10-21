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

(defn load-svg
  "Loads the SVG `fname` and parses the XML into a hiccup data structure, keeping all elements.
Use this function to load the SVG without throwing away any nodes, for example, if you want to keep meta and def tags."
  [fname]
  (-> fname
      slurp
      u/svg-str->hiccup))

(defn load-svg-elems
  "Loads the SVG `fname`, parses the XML into hiccup, and returns a sequence of the SVG elements in the file.
Use this function to pull elements from an SVG that can be used directly with the other functions in this library."
  ([fname] (load-svg-elems fname u/svg-element-keys))
  ([fname key-set]
   (-> fname
       slurp
       u/svg-str->hiccup
       (u/get-elems key-set))))

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
