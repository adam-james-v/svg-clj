(ns svg-clj.composites
  "Provides functions that combine transforms and primitive elements to make more complicated shapes.

  Additionally, the SVG container function is provided here as it relies on [[svg-clj.transforms]] to allow automatic veiwBox setup."
  (:require [clojure.string :as str]
            [svg-clj.utils :as utils]
            [svg-clj.elements :as svg]
            [svg-clj.transforms :as tf]))

(defn svg
   "Wraps `content` in an SVG container element whose width, height, and viewBox properties are automatically calculated when `w`, `h`, and `sc` are omitted.
   The SVG container is optionally parameterized by width `w`, height `h`, and scale `sc`."
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

(defn arrow
  ([a b]
   (let [tip-pts [ [0 0] [5 0] [5 5] ]
         tip-shape (svg/polygon tip-pts)]
     (arrow a b tip-shape)))

  ([a b tip-shape]
   (let [[mx my] (tf/centroid tip-shape)
         r (utils/to-deg (apply #(Math/atan2 %1 %2) (utils/v- b a)))]
     (->
      (svg/g
       (svg/line a b)
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
