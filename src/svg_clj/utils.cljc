(ns svg-clj.utils
  (:require [clojure.string :as str]
            #?(:cljs
               [cljs.reader :refer [read-string]])))

;; vector arithmetic helpers
(def v+ (partial mapv +))
(def v- (partial mapv -))
(def v* (partial mapv *))

;; simple calcs
(defn to-deg
  [rad]
  (* rad (/ 180 Math/PI)))

(defn to-rad
  [deg]
  (* deg (/ Math/PI 180)))

(defn round
  [num places]
  (let [d (Math/pow 10 places)]
    (/ (Math/round (* num d)) d)))

(defn average
  [& numbers]
  (let [n (count numbers)]
    (/ (apply + numbers) n)))
 
;; some string transformation tools
(defn v->s
  "Turns the vector `v` into a string formatted for use in SVG attributes."
  [v]
  (str/join "," v))

(defn s->v
  "Turns a string of comma or space separated numbers into a vector."
  [s]
  (-> s
      (str/trim)
      (str/split #"[, ]")
      (#(filter (complement empty?) %))
      (#(mapv read-string %))))

(defn xf-kv->str
  [[k v]]
  (str (symbol k) (apply list v)))

(defn str->xf-kv
  [s]
  (let [split (str/split s #"\(")
        key (keyword (first split))
        val (vec (read-string (str "(" (second split))))]
    [key val]))

(defn xf-map->str
  [m]
  (str/join "\n" (map xf-kv->str m)))

(defn str->xf-map
  [s]
  (if-let [s s]
    (into {} (map str->xf-kv (str/split-lines s)))
    {}))

;; geom
(defn distance
  "compute distance between two points"
  [a b]
  (let [v (v- b a)
        v2 (reduce + (v* v v))]
    (Math/sqrt v2)))

(defn move-pt
  [mv pt]
  (v+ pt mv))

(defn rotate-pt
  [deg [x y]]
  (let [c (Math/cos (to-rad deg))
        s (Math/sin (to-rad deg))]
    [(- (* x c) (* y s))
     (+ (* x s) (* y c))]))

(defn rotate-pt-around-center
  [deg center pt]
  (->> pt
       (move-pt (map - center))
       (rotate-pt deg)
       (move-pt center)))

(defn dot*
  "calculates the dot product of two vectors"
  [a b]
  (reduce + (map * a b)))
;; https://math.stackexchange.com/questions/361412/finding-the-angle-between-three-points
(defn angle-from-pts
  [p1 p2 p3]
  (let [v1 (v- p2 p1)
        v2 (v- p2 p3)
        l1 (distance p1 p2)
        l2 (distance p3 p2)
        n (dot* v1 v2)
        d (Math/abs (* l1 l2))]
    (when (not (= 0.0 (float d)))
      (to-deg (Math/acos (/ n d))))))
