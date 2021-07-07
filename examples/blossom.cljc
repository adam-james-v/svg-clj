(ns examples.blossom
  (:require [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.transforms :as tf]
            [svg-clj.composites :as comp :refer [svg]]
            [svg-clj.path :as path]
            [svg-clj.parametric :as p]
            [svg-clj.layout :as lo]
            [svg-clj.tools :as tools]
            #?(:clj [svg-clj.tools :as tools])))
            
(defn flip-y
  [pts]
  (mapv #(utils/v* % [1 -1]) pts))

(defn petal
  [cpts]
  (let [beza (apply path/bezier cpts)
        bezb (apply path/bezier (flip-y cpts))
        shape (tf/merge-paths beza bezb)
        ctr (tf/centroid shape)]
    (-> shape
        (tf/rotate -90)
        (tf/translate (utils/v* ctr [-1 -1])))))

(defn petal-ring
  [petal r n]
  (el/g
   (lo/distribute-on-curve
    (repeat n petal)
    (p/circle r))))

(def petal-01
  (-> (petal [[0 0] [5 -50] [50 -20] [75 0]])
      (tf/style {:fill "#ff8b94"
                 :stroke "#ffaaa5"
                 :stroke-width "4px"
                 :stroke-linecap "round"})))

(def petal-02
  (-> (petal [[0 0] [1 -20] [20 -10] [40 0]])
      (tf/style {:fill "#ffaaa5"
                 :stroke "none"})))

(def petal-03
  (-> (tf/merge-paths petal-01 petal-02)
      (tf/style {:fill "#a8e6cf"})))

(def petal-ring-01 (petal-ring petal-01 120 12))
(def petal-ring-02 (petal-ring petal-02 120 12))

(def petal-ring-03
  (-> (petal-ring petal-03 70 6)
      (tf/rotate (/ 360.0 24))))

(def petal-ring-04
  (let [petal (-> petal-03 (tf/style {:fill "#cc5963"}))]
    (-> (petal-ring petal 90 6)
        (tf/rotate (/ 360.0 24))
        (tf/rotate (/ 360.0 12)))))

(def petal-ring-05
  (let [petal (-> petal-02
                  (tf/rotate 180)
                  (tf/style {:fill "none"
                             :stroke "#f4f1d7"
                             :stroke-width "2px"}))]
    (-> (petal-ring petal 70 36)
        (tf/rotate (/ 360.0 24)))))

(def petal-ring-06
  (let [petal (-> petal-02
                  (tf/style {:fill "none"
                             :stroke "#f4f1d7"
                             :stroke-width "2px"}))]
    (-> (petal-ring petal 40 20)
        (tf/rotate (/ 360.0 24)))))

(def blossom (el/g
              (-> (el/circle 105) (tf/style {:fill "#69b599"}))
              petal-ring-01
              petal-ring-02
              petal-ring-06
              petal-ring-05
              petal-ring-04
              petal-ring-03))

;; when in a Clojure context, you can compile to SVG files
;; this uses the Hiccup html compiler
;; emitted SVG data works with Reagent as well.

(tools/save-svg blossom "examples/blossom.svg")
