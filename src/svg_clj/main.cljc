(ns svg-clj.main
  (:require [clojure.string :as s]
            [hiccup.core :refer [html]]
            [clojure.test :as test :refer [deftest is]]))

(defn svg
  "This function wraps `content` in an SVG container element.
  The SVG container is parameterized by width `w`, height `h`, and scale `sc`."
  [[w h sc] & content]
  [:svg {:width  w
         :height h
         :viewBox (str "0 0 " w " " h)
         :xmlns "http://www.w3.org/2000/svg"}
   [:g {:transform (str "scale(" sc ")")} content]])

(def svg-elements
  "The elements provided by the library."
  #{:circle
    :ellipse
    :line
    :path
    :polygon
    :polyline
    :rect
    :text
    :g})

(defn element? 
  "Checks the key in an element to see if it is an SVG element."
  [[k props content]]
  (svg-elements (first item)))

(defn circle
  [r]
  [:circle {:cx 0 :cy 0 :r r}])

(defn ellipse
  [rx ry]
  [:ellipse {:cx 0 :cy 0 :rx rx :ry ry}])

(defn line
  [[ax ay] [bx by]]
  [:line {:x1 ax :y1 ay :x2 bx :y2 by}])

(defn polygon
  [pts]
  [:polygon {:points (points->str pts)}])

(defn polyline
  [pts]
  [:polyline {:points (points->str pts)}])

(defn rect
  [w h]
  [:rect {:width w :height h :x (/ w -2.0) :y (/ h -2.0)}])

(defn text
  [text]
  (let [char-w 9.625
        char-h 10
        n-chars (count text)
        x (/ (* n-chars char-w) -2.0)
        y (/ char-h 2.0)]
    [:text {:x (/ (* n-chars char-w) -2.0)
            :y (/ char-h 2.0)
            :transform (xf-map->str {:rotate [0 (- x) (- y)]})
            :style {:font-family "monospace"
                    :font-size 16}} text]))

(defn g
  [& content]
  (if (and (= 1 (count content))
           (not (keyword? (first (first content)))))
    ;; content is a list of a list of elements
    (into [:g {}] (first content))
    ;; content is a single element OR a list of elements
    (into [:g {}] (filter (complement nil?) content))))

(defn average
  [& numbers]
  (let [n (count numbers)]
    (/ (apply + numbers) n)))

;; what I used to call 'midpoint' is more accurately called centroid
(defn centroid
  "Calculates the arithmetic mean position of all the given `pts`."
  [pts]
  (let [ndim (count (first (sort-by count pts)))
        splits (for [axis (range 0 ndim)]
                 (map #(nth % axis) pts))]
    (mapv #(apply average %) splits)))
        



;; midpoint arguably only refers to the middle point of a segment.

(defn path
  [d]
  [:path {:d d
          :fill-rule "evenodd"}])


