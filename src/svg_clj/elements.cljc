(ns svg-clj.elements
  (:require [clojure.string :as str]
            [svg-clj.utils :as utils]))

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
  [:polygon {:points (str/join " " (map utils/v->s pts))}])

(defn polyline
  [pts]
  [:polyline {:points (str/join " " (map utils/v->s pts))}])

(defn rect
  [w h]
  [:rect {:width w :height h :x (/ w -2.0) :y (/ h -2.0)}])

(defn image
  [url w h]
  [:image {:href url :width w :height h :x (/ w -2.0) :y (/ h -2.0)}])

(defn text
  [text]
  [:text {:x 0
          :y 0
          :font-size 12
          :text-anchor "middle"
          :dominant-baseline "middle"} text])

(defn g
  [& content]
  (if (and (= 1 (count content))
           (not (keyword? (first (first content)))))
    ;; content is a list of a list of elements
    (into [:g {}] (first content))
    ;; content is a single element OR a list of elements
    (into [:g {}] (filter (complement nil?) content))))
