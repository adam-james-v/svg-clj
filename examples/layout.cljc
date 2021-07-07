(ns examples.layout
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [hiccup.core :refer [html]]
            [svg-clj.composites :as cp :refer [svg]]
            [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.layout :as lo]
            [svg-clj.parametric :as p]
            #?(:clj [svg-clj.tools :as tools])))

(defn show-debug-geom
  [elem]
  (let [ctr (tf/centroid elem)
        bds (tf/bounds elem)]
    (el/g elem
       (el/g (-> (el/polygon bds)
               (tf/style {:fill "none"
                       :stroke "red"
                       :stroke-width "3px"}))
          (-> (el/circle 2)
               (tf/translate ctr)
               (tf/style {:fill "red"}))))))

(defn rand-rect
  []
  (let [w (+ 5 (rand-int 20)) 
        h (+ 5 (rand-int 20))
        origin [0 (/ h 2)]]
    (-> (el/rect w h)
        (tf/translate origin)
        (tf/style {:fill (str "rgb("
                              (rand-int 255) ","
                              (rand-int 255) ","
                              (rand-int 255) ")")}))))

;; Distribute a list of elements along X/Y axis, keeping a constant gap between the boundaries of each element.

(def horizontal-dist
  (el/g
   (map show-debug-geom
        (drop 2 (lo/distribute-linear :x 10 (repeatedly 7 rand-rect))))))

(def vertical-dist
  (el/g
   (map show-debug-geom
        (drop 2 (lo/distribute-linear :y 10 (repeatedly 7 rand-rect))))))

;; Distribute a list of elements onto a list of points.
;; Works like map, so whichever runs out first (elements or points) is the limiter.

(def grid-dist
 (lo/distribute-on-pts 
  (repeatedly rand-rect)
  (p/rect-grid 10 10 30 30)))

(def grid-dist2
 (lo/distribute-on-pts
  (repeat (el/circle 1.5))
  (p/rect-grid 20 30 10 10)))

;; Distribute a list of elements onto a parametric curve.
(def redline
  (-> (el/line [0 0] [0 15])
      (tf/style {:stroke "red" :stroke-width "3px"})))

(def circle-curve-dist
 (el/g
  (el/circle 150)
  (lo/distribute-on-curve
   (repeat 80 redline)
   (p/circle 150))))

(def circle-curve-dist2
 (el/g
  (el/circle 150)
  (lo/distribute-on-curve
   (repeatedly 40 rand-rect)
   (p/circle 150))))

;; Distribute on any curve available in parametric.cljc
(def bez-curve (p/bezier [ [70 -20] [10 70] [200 -300] [300 0]]))

(def bezier-curve-dist 
 (lo/distribute-on-curve
  (repeatedly 20 rand-rect)
  bez-curve))

(def examples [horizontal-dist
               vertical-dist
               grid-dist
               grid-dist2
               circle-curve-dist
               circle-curve-dist2
               bezier-curve-dist])

(def doc
  (->>
   (for [elem examples]
     (-> elem
         svg
         (tf/style {:style {:outline "1px solid blue"
                         :margin "10px"}})))
   (partition-all 3)
   (interpose [:br])))

(spit 
 "examples/layout.html"
 (html 
  [:html 
   [:body
    [:h1 "Layout Examples"]
    doc]]))
