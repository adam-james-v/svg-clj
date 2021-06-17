(ns svg-clj.layout
  (:require [clojure.string :as str]
            [svg-clj.elements :as svg]
            [svg-clj.utils :as utils]
            [svg-clj.transforms :as tf]))

(defn distribute-linear
  [axis-key gap items]
  (let [dir (axis-key {:x first :y second})
        distances
        (reductions +
                    (map #(+ (/ (dir (tf/bb-dims %1)) 2)
                             (/ (dir (tf/bb-dims %2)) 2) gap)
                         items (rest items)))]
    (svg/g
     (conj 
      (map
       #(tf/translate %1 (if (= axis-key :x) 
                           [%2 0]
                           [0 %2])) (rest items) distances)
      (first items)))))

(defn distribute-on-pts
  [items pts]
  (svg/g (map #(-> %1 (tf/translate %2)) items pts)))

(defn distribute-on-curve
  [items curve]
  (let [n (count items)
        step (/ 1.0 n)]
    (map #(-> %1 (tf/translate (curve %2))) items (range 0 1.0 step))))

(defn pattern-on-pts
  [item pts]
  (svg/g (map #(-> item (tf/translate %)) pts)))

(defn pattern-on-curve
  [item curve n]
  (let [step (/ 1.0 n)]
    (map #(-> item (tf/translate (curve %))) (range 0 1.0 step))))
