(ns svg-clj.layout
  "Provides functions for layout control of elements."
  (:require [svg-clj.elements :as el]
            [svg-clj.transforms :as tf]
            [svg-clj.utils :as u]))

(defn distribute-linear
  "Distribute `elems` along the `axis` (either :x or :y) with a `gap` distance between each item."
  [elems axis gap]
  (let [getfn (axis {:x first :y second})
        distances
        (reductions +
                    (map #(+ (/ (getfn (u/bb-dims %1)) 2)
                             (/ (getfn (u/bb-dims %2)) 2) gap)
                         elems (rest elems)))]
    (el/g
     (conj
      (map
       #(tf/translate %1 (if (= axis :x)
                           [%2 0]
                           [0 %2])) (rest elems) distances)
      (first elems)))))

(defn distribute-on-pts
  "Distribute the `elems` along the given `pts`. Each element is centered on its point."
  [elems pts]
  (el/g (map #(-> %1 (tf/translate %2)) elems pts)))

(defn distribute-on-curve
  "Distribute the `elems` evenly along the given `curve`."
  [elems curve]
  (let [eps u/*eps*
        n (if (> (count elems) 1) (dec (count elems)) 1)
        xf (fn [elem t]
             (let [t (cond (<= (- 1 eps) t) (- 1 eps)
                           (> eps t) eps
                           :else t)
                   n (u/normal (curve (- t eps)) (curve (+ t eps)))
                   a (u/angle-from-pts n [0 0] [0 1])
                   o (map #(u/round % 4) (u/rotate-pt (tf/centroid elem) a))]
               (-> elem
                   (tf/rotate a)
                   (tf/translate (u/v- (curve t) o (tf/centroid elem))))))]
    (map #(xf %1 (float (/ %2 n))) elems (range 0 (inc n)))))

(defn pattern-on-pts
  "Repeat `elem`, centering on each point of `pts`."
  [elem pts]
  (el/g (map #(-> elem (tf/translate %)) pts)))

(defn pattern-on-curve
  "Repeat `elem` evenly along `curve` `n` times."
  [elem curve n]
  (let [step (/ 1.0 n)]
    (map #(-> elem (tf/translate (curve %))) (range 0 1.0 step))))
