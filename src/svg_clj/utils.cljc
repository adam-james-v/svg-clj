(ns svg-clj.utils
  (:require [clojure.string :as str]
            [same :refer [zeroish?]]
            #?(:clj
               [clojure.data.xml :as xml])
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
    (into {} 
          (->> s
               (#(str/replace % #"\)" ")\n"))
               str/split-lines
               (map str/trim)
               (map str->xf-kv)))
    {}))

(defn distance
  "compute distance between two points"
  [a b]
  (let [v (v- b a)
        v2 (reduce + (v* v v))]
    (Math/sqrt v2)))

(defn rotate-pt
  [[x y] deg]
  (let [c (Math/cos (to-rad deg))
        s (Math/sin (to-rad deg))]
    [(- (* x c) (* y s))
     (+ (* x s) (* y c))]))

(defn rotate-pt-around-center
  [pt deg center]
  (-> pt
      (v+ (map - center))
      (rotate-pt deg)
      (v+ center)))

(defn dot*
  "calculates the dot product of two vectors"
  [a b]
  (reduce + (map * a b)))

(defn cross*
  "calculates cross product of two 3d-vectors"
  [a b]
  (let [[a1 a2 a3] a
        [b1 b2 b3] b
        i (- (* a2 b3) (* a3 b2))
        j (- (* a3 b1) (* a1 b3))
        k (- (* a1 b2) (* a2 b1))]
    [i j k]))

(defn normalize
  "find the unit vector of a given vector"
  [v]
  (let [m (Math/sqrt (reduce + (v* v v)))]
    (mapv / v (repeat m))))

(defn normal
  "Find normal vector of plane given 3 points. Find normal vector of line given two (2D) points."
  ([a b]
   (let [[x1 y1] a
         [x2 y2] b
         dx (- x2 x1)
         dy (- y2 y1)]
     [(- dy) dx]))
  ([a b c]
   (let [ab (mapv - a b)
         ac (mapv - a c)]
     (cross* ab ac))))

;; https://math.stackexchange.com/questions/361412/finding-the-angle-between-three-points
(defn- check-quadrants
  [p1 p2 p3]
  (let [v1 (v- p1 p2)
        v2 (v- p3 p2)
        qf (fn [[x y]]
             (cond (and (pos? x) (pos? y)) "pp"
                   (and (pos? x) (neg? y)) "pn"
                   (and (neg? x) (neg? y)) "nn"
                   (and (neg? x) (pos? y)) "np"
                   (pos? x) "p_"
                   (neg? x) "n_"
                   (pos? y) "_p"
                   (neg? y) "_n"))]
    (apply str (map qf [v1 v2]))))

(defn angle-from-pts
  [p1 p2 p3]
  (let [v1 (v- p1 p2)
        v2 (v- p3 p2)
        l1 (distance p1 p2)
        l2 (distance p3 p2)
        n (dot* v1 v2)
        d (* l1 l2)
        q (if (#{"nnnn" "npnp" "_pnn" "_pnp" "_pn_"} (check-quadrants p1 p2 p3)) 1 -1)]
    (when (not (zeroish? (float d)))
      (* q (to-deg (Math/acos (/ n d)))))))

(defn determinant
  [a b]
  (- (* (first a) (second b))
     (* (second a) (first b))))


;; this fn name doesn't make sense? It inverts y, which is not
;; the same as giving a perpendicular line
;; maybe call it 'invert-y' or 'vertical-flip'
(defn perpendicular
  [[x y]]
  [(- y) x])

(defn line-intersection
  [[a b] [c d]]
  (let [[ax ay] a
        [bx by] b
        [cx cy] c
        [dx dy] d
        xdiff [(- ax bx) (- cx dx)]
        ydiff [(- ay by) (- cy dy)]
        div (determinant xdiff ydiff)]
    (when (not (zeroish? (Math/abs div)))
      (let [d [(determinant a b) (determinant c d)]
            x (/ (determinant d xdiff) div)
            y (/ (determinant d ydiff) div)]
        [x y]))))

(defn xml->hiccup [xml]
  (if-let [t (:tag xml)]
    (let [elt [t]
          elt (if-let [attrs (:attrs xml)]
                (conj elt attrs)
                elt)]
      (into elt (map xml->hiccup (:content xml))))
    xml))

(defn svg-str->elements
  [svg-str]
  (-> svg-str
      (xml/parse-str :namespace-aware false)
      xml->hiccup))
