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
  (-> (el/rect (+ 5 (rand-int 20)) (+ 5 (rand-int 20)))
      (tf/style {:fill (str "rgb("
                            (rand-int 255) ","
                            (rand-int 255) ","
                            (rand-int 255) ")")})))

(def dwg
  (svg
   (lo/distribute-linear :x 10 (repeatedly 9 rand-rect))))
