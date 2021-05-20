(ns examples.layout
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [hiccup.core :refer [html]]
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

(tools/cider-show (lo/distribute-linear :x 0 (repeatedly 7 rand-rect)))
