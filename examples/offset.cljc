(ns examples.offset
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [hiccup.core :refer [html]]
            [svg-clj.composites :as cp :refer [svg]]
            [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.parametric :as p]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.layout :as lo]
            #?(:clj [svg-clj.tools :as tools])))

(def a (-> (p/regular-polygon-pts 120 10)
           (el/polygon)
           (tf/style {:fill "none"
                      :stroke "red"
                      :stroke-width "3px"})))

(def b (-> (tf/offset a 20)
           (tf/style {:fill "none"
                      :stroke "limegreen"
                      :stroke-width "3px"})))

(def c (-> (el/circle 40)
           (tf/style {:fill "none"
                      :stroke "red"
                      :stroke-width "3px"})))

(def d (-> (tf/offset c -3)
           (tf/style {:fill "none"
                      :stroke "limegreen"
                      :stroke-width "3px"})))

(tools/cider-show (el/g a b))
(tools/cider-show (el/g c d))

(def pts [ [0 0] [100 -300] [200 -300] [300 0]])
(def curve (p/bezier pts))

(def e (-> (map curve (range 0 1.05 0.05))
           (el/polyline)
           (tf/style {:fill "none"
                      :stroke "red"
                      :stroke-width "3px"})))

(def f (-> (map curve (range 0 1.05 0.05))
           (tf/offset-pts 20)
           (el/polyline)
           (tf/style {:fill "none"
                      :stroke "limegreen"
                      :stroke-width "3px"})))

(tools/cider-show (el/g e f))
