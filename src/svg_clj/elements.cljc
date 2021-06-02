(ns svg-clj.elements
  (:require [clojure.string :as str]
            [svg-clj.utils :as utils]
            [svg-clj.transforms :as tf]))

(defn svg
   "The svg fn wraps `content` in an SVG container element.
   The SVG container is parameterized by width `w`, height `h`, and scale `sc`."
  ([content]
   (let [[w h] (tf/bb-dims content)
         [[x y] _ _ _] (tf/bounds content)]
     [:svg {:width  w
            :height h
            :viewBox (str/join " " [x y w h])
            :xmlns "http://www.w3.org/2000/svg"}
      content]))

  ([content w h]
   [:svg {:width  w
          :height h
          :viewBox (str "0 0 " w " " h)
          :xmlns "http://www.w3.org/2000/svg"}
    content])

  ([content w h sc]
   (svg [:g {:transform (str "scale(" sc ")")} content] w h)))

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

(defn arrow
  ([a b]
   (let [tip-pts [ [0 0] [5 0] [5 5] ]
         tip-shape (polygon tip-pts)]
     (arrow a b tip-shape)))

  ([a b tip-shape]
   (let [[mx my] (tf/centroid tip-shape)
         r (utils/to-deg (apply #(Math/atan2 %1 %2) (utils/v- b a)))]
     (->
      (g
       (line a b)
       (-> tip-shape
           (tf/translate [(- mx) (- my)])
           (tf/rotate (- 315 r))
           (tf/translate a)
           (tf/style {})
           (tf/style {:fill "none"
                      :stroke "none"}))
       (-> tip-shape
           (tf/translate [(- mx) (- my)])
           (tf/rotate (- 135 r))
           (tf/translate b)))))))

(defn label
  [font-size text]
  [:text 
   {:x 0 :y 0 
    :style {:font-family "Verdana"
            :text-anchor "middle"
            :dominant-baseline "middle"
            :font-size font-size}} text])
