(ns examples.layout
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [hiccup.core :refer [html]]
            [svg-clj.composites :as cp :refer [svg]]
            [svg-clj.utils :as utils]
            [svg-clj.elements :as svg]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.layout :as lo]
            #?(:clj [svg-clj.tools :as tools])))

(defn show-debug-geom
  [elem]
  (let [ctr (tf/centroid elem)
        bds (tf/bounds elem)]
    (svg/g elem
       (svg/g (-> (svg/polygon bds)
               (tf/style {:fill "none"
                       :stroke "red"
                       :stroke-width "3px"}))
          (-> (svg/circle 2)
               (tf/translate ctr)
               (tf/style {:fill "red"}))))))

(defn rand-rect
  []
  (-> (svg/rect (+ 5 (rand-int 20)) (+ 5 (rand-int 20)))
      (tf/style {:fill (str "rgb("
                            (rand-int 255) ","
                            (rand-int 255) ","
                            (rand-int 255) ")")})))

(tools/cider-show (map show-debug-geom (drop 2 (lo/distribute-linear :x 10 (repeatedly 7 rand-rect)))))

(tools/cider-show (map show-debug-geom (drop 2 (lo/distribute-linear :y 10 (repeatedly 7 rand-rect)))))

(tools/cider-show 
 (lo/distribute-on-pts 
  (repeatedly rand-rect)
  (lo/rect-grid 10 10 30 30)))

(tools/cider-show 
 (lo/distribute-on-curve
  (repeatedly 40 rand-rect)
  (lo/p-circle 150)))

(tools/cider-show 
 (lo/distribute-linear
  :x
  20
  (repeatedly 10 rand-rect)))

(ns examples.bezier
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [hiccup.core :refer [html]]
            [svg-clj.composites :as cp :refer [svg]]
            [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.layout :as lo]
            #?(:clj [svg-clj.tools :as tools])))

(def pts [ [0 0] [80 80] [160 -20] ])

(def b
  (let [c (lo/p-bezier pts)
        cpts (for [t (range 0 1 0.1)]
               (c t))]
    (el/g
     (map #(-> (el/circle 2.5)
               (tf/translate %)
               (tf/style {:fill "red"}))
          cpts))))

(def a (-> (apply path/bezier pts)
           (tf/style {:fill "none"
                      :stroke-width "2px"
                      :stroke "pink"})))

(def c (el/g a b))

(def d (tools/load-svg "/Users/adam/dev/forge/sk.svg"))
