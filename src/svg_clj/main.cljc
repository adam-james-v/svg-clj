(ns svg-clj.main
  (:require [clojure.string :as str]
            #?(:clj [clojure.data.xml :as xml])
            [svg-clj.utils :as utils]))

(defn svg
   "This fn wraps `content` in an SVG container element.
   The SVG container is parameterized by width `w`, height `h`, and scale `sc`."
  [[w h sc] & content]
  [:svg {:width  w
         :height h
          :viewBox (str "0 0 " w " " h)
         :xmlns "http://www.w3.org/2000/svg"}
   (if sc
     [:g {:transform (str "scale(" sc ")")} content]
     content)])

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

(defn g
  [& content]
  (if (and (= 1 (count content))
           (not (keyword? (first (first content)))))
    ;; content is a list of a list of elements
    (into [:g {}] (first content))
    ;; content is a single element OR a list of elements
    (into [:g {}] (filter (complement nil?) content))))

(defn text
  [text]
  [:text {:x 0 :y 0} text])

(defn style
  [style [k props & content]]
  (into [k (merge props style)] content))
