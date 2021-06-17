(ns drawing.main
  (:require [clojure.string :as str]
            [svg-clj.composites :as cp :refer [svg]]
            [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.layout :as lo]))

(defn rand-rect
  []
  (-> (el/rect 10 #_(+ 5 (rand-int 20)) 10 #_(+ 5 (rand-int 20)))
      (tf/style {:fill (str "rgb("
                            (+ 100 (rand-int 150)) ","
                            (+ 100 (rand-int 150)) ","
                            (+ 100 (rand-int 150)) ")")})))

#_(def dwg
  (svg
   (lo/distribute-linear :y 10 (repeatedly 20 rand-rect))))

(def dwg2
  (svg
   (el/g
    (for [step (range 1 9 1)]
      (lo/distribute-on-curve
       (repeatedly (* 10 step) rand-rect)
       (lo/p-circle (* 50 step)))))))
