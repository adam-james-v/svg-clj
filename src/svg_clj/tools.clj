(ns svg-clj.tools
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [hiccup.core :refer [html]]
            [hawk.core :as hawk]
            [svg-clj.elements :as svg]
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
               (svg/svg svg-data))]
    (do (png! data fname)
        (clojure.java.io/file fname))))

(defn watch!
  [fname]
  (let [ [name ext] (str/split fname #"\.")]
    (hawk/watch!
     [{:paths [fname]
       :handler
       (fn [ctx e]
         (require '[svg-clj.elements :refer :all]
                  '[svg-clj.transforms :refer :all]
                  '[svg-clj.path :refer :all]
                  '[hiccup.core :refer [html]])
         (->> (slurp fname)
              (format "[%s]")
              load-string
              (filter (complement var?))
              html
              (spit (str name ".html")))
         ctx)}])))
