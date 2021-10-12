(ns svg-clj.layout
  "Provides functions for layout control of elements."
  (:require [svg-clj.elements :as el]
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
    (el/g
     (conj 
      (map
       #(tf/translate %1 (if (= axis-key :x) 
                           [%2 0]
                           [0 %2])) (rest items) distances)
      (first items)))))

(defn distribute-on-pts
  [items pts]
  (el/g (map #(-> %1 (tf/translate %2)) items pts)))

(defn distribute-on-curve
  [items curve]
  (let [eps 0.000001
        n (if (> (count items) 1) (dec (count items)) 1)
        xf (fn [item t]
             (let [t (cond (<= (- 1 eps) t) (- 1 eps)
                           (> eps t) eps
                           :else t)
                   n (utils/normal (curve (- t eps)) (curve (+ t eps)))
                   a (utils/angle-from-pts n [0 0] [0 1])
                   o (map #(utils/round % 4) (utils/rotate-pt (tf/centroid item) a))]
               (-> item
                   (tf/rotate a)
                   (tf/translate (utils/v- (curve t) o (tf/centroid item))))))]
    (map #(xf %1 (float (/ %2 n))) items (range 0 (inc n)))))

(defn pattern-on-pts
  [item pts]
  (el/g (map #(-> item (tf/translate %)) pts)))

(defn pattern-on-curve
  [item curve n]
  (let [step (/ 1.0 n)]
    (map #(-> item (tf/translate (curve %))) (range 0 1.0 step))))
