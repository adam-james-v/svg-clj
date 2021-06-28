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

(tools/cider-show (map show-debug-geom (drop 2 (lo/distribute-linear :x 10 (repeatedly 7 rand-rect)))))

(tools/cider-show (map show-debug-geom (drop 2 (lo/distribute-linear :y 10 (repeatedly 7 rand-rect)))))

(tools/cider-show 
 (lo/distribute-on-pts 
  (repeatedly rand-rect)
  (lo/rect-grid 10 10 30 30)))

(def pts [ [70 -20] [10 70] [200 -300] [300 0]])
(def curve (p/bezier pts))
(def asdf (-> (el/line [0 0] [0 15])
              (tf/style {:stroke "red" :stroke-width "3px"})))

(tools/cider-show
 (el/g
  (el/circle 150)
  (lo/distribute-on-curve
   (repeat 80 asdf)
   (p/circle 150))))

(tools/cider-show 
 (lo/distribute-on-curve
  (repeatedly 20 rand-rect)
  curve))

(tools/cider-show 
 (lo/distribute-on-curve
  (repeatedly 40 rand-rect)
  (p/circle 150)))

(def city
  (let [buildings (lo/distribute-on-curve
                   (repeatedly 100 rand-rect)
                   (p/circle 150))
        paths (map tf/element->path buildings)
        skyline (apply tf/merge-paths paths)]
    (-> skyline
        (tf/style {:fill "none"
                   :stroke "hotpink"}))))

(tools/cider-show 
 (lo/distribute-linear
  :x
  3
  (repeatedly 10 rand-rect)))

(tools/cider-show 
 (lo/distribute-on-pts
  (repeat (el/circle 1))
  (p/rect-grid 20 30 10 10)))
