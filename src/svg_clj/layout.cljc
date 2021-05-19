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

(defn rand-rect
  []
  (-> (svg/rect (+ 5 (rand-int 20)) (+ 5 (rand-int 20)))
      (tf/style {:fill (str "rgb("
                            (rand-int 255) ","
                            (rand-int 255) ","
                            (rand-int 255) ")")})))

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
