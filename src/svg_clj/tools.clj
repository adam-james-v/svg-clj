(ns svg-clj.tools
  (:require [clojure.java.browse]
            [clojure.java.io]
            [clojure.zip :as zip]
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
  [fname]
  (-> fname
      slurp
      utils/svg-str->elements))

(defn- get-nodes
  "Returns a list of nodes from `zipper` that return `true` from the `matcher` predicate fn.
  The `matcher` fn expects a zipper location, `loc`, and returns `true` (or some value) or `false` (or nil)."
  [zipper matcher]
  (loop [loc zipper
         acc []]
    (if (zip/end? loc)
      acc
      (if (matcher loc)
        (recur (zip/next loc) (conj acc (zip/node loc)))
        (recur (zip/next loc) acc)))))

(defn- elem-node?
  [loc key-set]
  (let [node (zip/node loc)]
    (if (keyword? (first node))
      (not (nil? (key-set (first node)))))))

(defn hiccup-zip
  [tree]
  (let [branch? #(and (seqable? %) (not (map? %)) (not (string? %)))
        children (fn [x]
                   (let [c (remove map? (rest x))]
                     (when-not (empty? c) c)))
        make-node (fn [_ c] (when-not (empty? c) (vec c)))]
    (zip/zipper branch? children make-node tree)))
  
(defn get-elems
  ([tree] (get-elems tree el/svg-element-keys))
  ([tree key-set]
   (let [zipper (hiccup-zip tree)]
    (apply list (get-nodes zipper #(elem-node? % key-set))))))

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
