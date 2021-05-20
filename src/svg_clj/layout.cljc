(ns svg-clj.layout
  (:require [clojure.string :as str]
            [svg-clj.elements :as svg]
            [svg-clj.utils :as utils]
            [svg-clj.transforms :as tf]))

(defn regular-polygon-pts
  [r n]
  (let [angle (* 2 Math/PI (/ 1 n))]
    (map #(vector (utils/round (* r (Math/cos (* % angle))) 5)
                  (utils/round (* r (Math/sin (* % angle))) 5))
         (range n))))

(defn rect-grid
  [nx ny w h]
  (for [a (range nx)
        b (range ny)]
    [(* w a) (* h b)]))

(defn p-line
  [a b]
  (fn [t]
    (cond
      (= (float t) 0.0) a
      (= (float t) 1.0) b
      :else
      (utils/v+ a (utils/v* (utils/v- b a) (repeat t))))))

(defn- radius-from-pts
  "compute the radius of an arc defined by 3 points"
  [p1 p2 p3]
  (let [a (utils/distance p3 p2)
        b (utils/distance p3 p1)
        c (utils/distance p2 p1)
        s (/ (+ a b c) 2)
        sa ( - s a)
        sb ( - s b)
        sc ( - s c)
        rt (Math/sqrt (* s sa sb sc))
        radius (/ (/ (* a b c) 4) rt)]
    radius))

(defn- center-from-pts
  "compute the center point of an arc through 3 points"
  [p1 p2 p3]
  (let [u1 (utils/v- p2 p1)
        u2 (utils/v- p3 p1)
        w1 (utils/cross* (utils/v- p3 p1) u1)
        u (utils/normalize u1)
        w (utils/normalize w1)
        v (utils/cross* w u)
        [bx by] [(utils/dot* u1 u) 0]
        [cx cy] [(utils/dot* u2 u) (utils/dot* u2 v)]
        h (/ (+ (Math/pow (- cx (/ bx 2)) 2) 
                (Math/pow cy 2)
                (- (Math/pow (/ bx 2) 2))) 
             (* 2 cy))]
    (utils/v+ p1
              (utils/v* (repeat (/ bx 2)) u) 
              (utils/v* (repeat h) v))))

(defn- angle-from-pts
  [p1 p2 p3]
  (let [v1 (utils/v- p2 p1)
        v2 (utils/v- p2 p3)
        l1 (utils/distance p1 p2)
        l2 (utils/distance p3 p2)
        n (utils/dot* v1 v2)
        d (Math/abs (* l1 l2))]
    (when (not (= 0.0 (float d)))
      (utils/to-deg (Math/acos (/ n d))))))

(defn p-circle
  ([r]
   (fn [t]
     (let [t (* 2 Math/PI t)
           x (* r (Math/cos t))
           y (* r (Math/sin t))]
       [x y])))

  ([a b c]
   (let [[a b c] (map #(conj % 0) [a b c])
         n (utils/normalize (utils/normal a b c))
         r (radius-from-pts a b c)
         cp (center-from-pts a b c)
         u (utils/normalize (utils/v- a cp))
         v (utils/cross* n u)]
     (fn [t]
       (cond
         (or (< t 0.0) (> t 1.0)) nil
         (= (float t) 0.0) (vec (drop-last a))
         (= (float t) 1.0) (vec (drop-last a))
         :else
         (let [t (* 2 Math/PI t)]
           (mapv 
            #(utils/round % 5)
            (drop-last 
             (utils/v+ cp
                       (utils/v* (repeat (* r (Math/cos t))) u)
                       (utils/v* (repeat (* r (Math/sin t))) v))))))))))

(defn p-ellipse
  [rx ry]
  (fn [t]
    (let [t (* 2 Math/PI t)
          x (* rx (Math/cos t))
          y (* ry (Math/sin t))]
      [x y])))

(defn p-arc
  [a b c]
  (let [[a b c] (map #(conj % 0) [a b c])
        f (p-circle a b c)
        cp (center-from-pts a b c)
        angle (angle-from-pts a cp c)]
    (fn [t]
      (let [t (* t (/ angle 360.0))]
        (f t)))))

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
