(ns svg-clj.elements
  "Provides functions to generate the renderable SVG elements.
  Every function in this namespace emits hiccup style data structures, and have the following shape: `[:tag {:prop \"value\"}]`, except g (group) and text which emit: `[:tag {:prop \"value\"} \"content\"]`.

  All functions in this namespace emit the primitive elements of an SVG image. These primitives are the basis for further manipulation using transform functions from [[svg-clj.transforms]].

  One notable element which is not provided is `path`. Since path elements have a more complex property specification, the [[svg-clj.path]] namespace is dedicated to path element generation."
  (:require [clojure.string :as str]
            [svg-clj.utils :as u]))

(defn circle
  "Emits a circle element with radius `r` centered at the origin."
  [r]
  [:circle {:cx 0 :cy 0 :r r}])

(defn ellipse
  "Emits an ellipse element with x-axis radius `rx` and y-axis radius `ry` centered at the origin."
  [rx ry]
  [:ellipse {:cx 0 :cy 0 :rx rx :ry ry}])

(defn line
  "Emits a line element starting at 2d point `pt-a` and ending at 2d point `pt-b`."
  [pt-a pt-b]
  (let [[ax ay] pt-a
        [bx by] pt-b]
    [:line {:x1 ax :y1 ay :x2 bx :y2 by}]))

(defn polygon
  "Emits a polygon element with 2d points from vector or list `pts`.
  Polygon elements have a closed path."
  [pts]
  [:polygon {:points (str/join " " (map u/v->s pts))}])

(defn polyline
  "Emits a polyline element with 2d points from vector or list `pts`.
  Polyline elements have an open path."
  [pts]
  [:polyline {:points (str/join " " (map u/v->s pts))}])

(defn rect
  "Emits a rect element of width `w` and height `h` centered at the origin."
  [w h]
  [:rect {:width w :height h :x (/ w -2.0) :y (/ h -2.0)}])

(defn image
  "Emits an image element of the image specified at `url`, of width `w`, and height `h` centered at the origin."
  [url w h]
  [:image {:href url :width w :height h :x (/ w -2.0) :y (/ h -2.0)}])

(defn text
  "Emits a text element containing `text` of font-size 12pt.
  By default, text is centered at the origin by setting text-anchor='middle' and dominant-baseline='middle'. These defaults can be changed using [[svg-clj.transforms/style]] to override any preset properties."
  [text]
  [:text {:x 0
          :y 0
          :font-size 12
          :text-anchor "middle"
          :dominant-baseline "middle"} text])

(defn g
  "Emits a g (group) element."
  [& content]
  (if (and (= 1 (count content))
           (not (keyword? (first (first content)))))
    ;; content is a list of a list of elements
    (into [:g {}] (first content))
    ;; content is a single element OR a list of elements
    (into [:g {}] (filter (complement nil?) content))))
