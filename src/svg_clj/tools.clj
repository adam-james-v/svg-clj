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
            [batik.rasterize :as b]
            [svg-clj.utils :as utils]))

(defn sh-png! [svg-data fname]
  (sh "convert" "-background" "none" "/dev/stdin" fname
      :in (html svg-data)))

(defn png! [svg-data fname]
  (b/render-svg-string (html svg-data) fname))

(defn cider-show
  [svg-data]
  (let [fname "_imgtmp.png"
        data (if (= (first svg-data) :svg)
               svg-data
               (svg svg-data))]
    (do (png! data fname)
        (clojure.java.io/file fname))))

(defn show
  [svg-data]
  (let [fname "_tmp.html"
        data (if (= (first svg-data) :svg)
               svg-data
               (svg svg-data))]
    (do (spit fname (html data))
        (clojure.java.browse/browse-url fname)
        #_(sh "rm" fname))))

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
