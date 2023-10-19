(ns svg-clj.utils
  (:require [clojure.string :as str]
            #?(:cljs [cljs.reader :refer [read-string]])))

#_(defn abs
  [x]
  (Math/abs x))

(def ^:dynamic *eps*
  "Epsilon Value where any floating point value less than `*eps*` will be considered zero."
  0.00001)

(defn zeroish?
  "`True` if the absolute value of number `x` is less than `*eps*`, which is 0.00001 by default."
  [x]
  (< (abs x) *eps*))

(def ^:dynamic *rounding-decimal-places*
  "The number of decimal places the `round` funciton will round to."
  5)

(def pow
  "Implementation for clj/cljs `pow` function."
  #?(:clj #(Math/pow %1 %2)
     :cljs js/Math.pow))

(defn round
  "Rounds a non-integer number `num` to `places` decimal places."
  ([num]
   (round num *rounding-decimal-places*))
  ([num places]
   (if places
     (let [d #?(:clj (bigdec (Math/pow 10 places))
                :cljs (Math/pow 10 places))]
       (double (/ (Math/round (* (double num) d)) d)))
     num)))

;; vector arithmetic helpers
(def v+ "Add vectors element-wise." (partial mapv +))
(def v- "Subtract vectors element-wise." (partial mapv -))
(def v* "Multiply vectors element-wise." (partial mapv *))

;; simple calcs
(defn to-deg
  "Convert `rad` radians to degrees."
  [rad]
  (round (* rad (/ 180 Math/PI))))

(defn to-rad
  "Convert `deg` degrees to radians."
  [deg]
  (round (* deg (/ Math/PI 180))))

(defn average
  "Compute the average of `numbers`."
  [& numbers]
  (let [n (count numbers)]
    (round (/ (apply + numbers) n))))

;; some string transformation tools
(defn v->s
  "Turns the vector `v` into a string with commas separating the values."
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

(defn- xf-kv->str
  "Formats a key value pair [`k` `v`] from a transform map into an inline-able string.
  Example:

  [:rotate [0 90 0]] -> \"rotate(0 90 0)\""
  [[k v]]
  (str (symbol k) (apply list v)))

(defn- str->xf-kv
  "Formats an SVG transform string `s` into a key value pair. The opposite of `xf-kv->str`.
  Example:

  \"rotate(0 90 0)\" -> [:rotate [0 90 0]]"
  [s]
  (let [split (str/split s #"\(")
        key (keyword (first split))
        val (vec (read-string (str "(" (second split))))]
    [key val]))

(defn xf-map->str
  "Turn transform maps from an element's properties into a string properly formatted for use inline in an svg element tag. Consider this an internal tool."
  [m]
  (str/join "\n" (map xf-kv->str m)))

(defn str->xf-map
  "Turn inline SVG transform strings from an element's properties into a map in a form which the transforms namespace expects. Consider this an internal tool."
  [s]
  (if-let [s s]
    (into {}
          (->> s
               (#(str/replace % #"\)" ")\n"))
               str/split-lines
               (map str/trim)
               (map str->xf-kv)))
    {}))

(defn rotate-pt
  "Rotates 2d point `pt` around the origin by `deg` in the counter-clockwise direction."
  [pt deg]
  (let [[x y] pt
        c (Math/cos (to-rad deg))
        s (Math/sin (to-rad deg))]
    [(round (- (* x c) (* y s)))
     (round (+ (* x s) (* y c)))]))

(defn rotate-pt-around-center
  "Rotates point `pt` around `center` by `deg` in the counter-clockwise direction."
  [pt deg center]
  (-> pt
      (v+ (map - center))
      (rotate-pt deg)
      (v+ center)))

(defn distance
  "Computes the distance between two points `a` and `b`."
  [a b]
  (let [v (v- b a)
        v2 (reduce + (v* v v))]
    (round (Math/sqrt ^double v2))))

(defn distance-squared
  "Computes the squared distance between two points `a` and `b`. Avoids a square-root calculation, so this can be used in some cases for optimization."
  [a b]
  (let [v (v- b a)]
    (reduce + (v* v v))))

(defn determinant
  "Computes the determinant between two 2D points `a` and `b`."
  [[a b] [c d]]
  (- (* a d)
     (* b c)))

(defn perpendicular
  "Returns a vector perpendicular to the vector [`x` `y`]."
  [[x y]]
  [(- y) x])

(defn dot*
  "Calculates the dot product of two vectors."
  [a b]
  (reduce + (map * a b)))

(defn cross*
  "Calculates cross product of two 3d-vectors. If `a` and `b` are 2D, z is assumed to be 0."
  [a b]
  (let [[a1 a2 a3] a
        [b1 b2 b3] b
        a3 (if a3 a3 0)
        b3 (if b3 b3 0)
        i (- (* a2 b3) (* a3 b2))
        j (- (* a3 b1) (* a1 b3))
        k (- (* a1 b2) (* a2 b1))]
    [i j k]))

(defn cross*-k
  "Calculates the k component of the cross product of two 2D vectors, assuming Z=0 as the 3rd component."
  [[ax ay] [bx by]]
  (- (* ax by) (* ay bx)))

(defn normal
  "Calculates the normal vector of plane given 3 points or calculates the normal vector of a line given two (2D) points."
  ([a b]
   (let [[x1 y1] a
         [x2 y2] b
         dx (- x2 x1)
         dy (- y2 y1)]
     [(- dy) dx]))
  ([a b c]
   (let [ab (v- a b)
         ac (v- a c)
         [x y z] (cross* ab ac)]
     (when (and (> x *eps*) (> y *eps*) (> z *eps*))
       [x y z]))))

(defn normalize
  "find the unit vector of a given vector"
  [v]
  (when v
    (let [m (Math/sqrt ^double (reduce + (v* v v)))]
      (mapv / v (repeat m)))))

;; https://math.stackexchange.com/questions/361412/finding-the-angle-between-three-points
(defn- check-quadrants
  "Using `p2` as the 'origin', return a string indicating positive, negative, or axis-aligned for p1 p2."
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
  "Calculates the angle starting at line p3p2 going to line p1p2.
Put another way, the angle is measured following the 'right hand rule' around p2."
  [p1 p2 p3]
  (let [v1 (v- p1 p2)
        v2 (v- p3 p2)
        [v1nx _] (normalize v1)
        [v2nx _] (normalize v2)
        l1 (distance p1 p2)
        l2 (distance p3 p2)
        n (dot* v1 v2)
        d (* l1 l2)]
    (when-not (zeroish? (float d))
      (let [a (to-deg (Math/acos (/ n d)))
            quadrants (check-quadrants p1 p2 p3)]
        (cond
          ;; same quadrant, checking if V2 is before or after V1
          (and (= "pppp" quadrants) (> v2nx v1nx)) a
          (and (= "npnp" quadrants) (> v2nx v1nx)) a
          (and (= "nnnn" quadrants) (< v2nx v1nx)) a
          (and (= "pnpn" quadrants) (< v2nx v1nx)) a
          ;; within same quadrant
          (#{"p_p_" "ppp_" "_ppp" "p_pn"} quadrants) a
          (#{"_p_p" "np_p" "n_np"} quadrants) a
          (#{"n_n_" "nnn_" "_nnn"} quadrants) a
          (#{"_n_n" "pn_n" "pnp_"} quadrants) a
          ;; one quadrant away
          (#{"npp_" "nn_p" "pnn_" "pp_n"} quadrants) a
          (#{"n_pp" "_nnp" "p_nn" "_ppn"} quadrants) a
          (#{"nppp" "nnnp" "pnnn" "pppn"} quadrants) a
          ;; 90 degrees away on axes
          (#{"_pp_" "n__p" "_nn_" "p__n"} quadrants) a
          ;; two quadrants away
          (and (= "ppnn" quadrants) (> (abs v1nx) (abs v2nx))) a
          (and (= "nnpp" quadrants) (> (abs v1nx) (abs v2nx))) a
          (and (= "pnnp" quadrants) (< (abs v1nx) (abs v2nx))) a
          (and (= "nppn" quadrants) (< (abs v1nx) (abs v2nx))) a
          ;; 180 degrees away on axes
          (#{"p_n_" "_p_n" "n_p_" "_n_p"} quadrants) a
          :else (- 360 a))))))

(defn line-intersection
  "Returns the intersection point between two 2D lines or `nil` if the lines are (close to) parallel. Assumes lines are infinite, so the intersection may lie beyond the line segments specified."
  [[pt-a pt-b] [pt-c pt-d]]
  (let [[ax ay] pt-a
        [bx by] pt-b
        [cx cy] pt-c
        [dx dy] pt-d
        xdiff [(- ax bx) (- cx dx)]
        ydiff [(- ay by) (- cy dy)]
        div (determinant xdiff ydiff)]
    (when-not (zeroish? (abs div))
      (let [dets [(determinant pt-a pt-b) (determinant pt-c pt-d)]
            x (/ (determinant dets xdiff) div)
            y (/ (determinant dets ydiff) div)]
        [x y]))))

(defn colinear?
  "`True` if points `a`, `b`, and `c` are along the same line."
  [a b c]
  (let [ba (v- a b)
        bc (v- c b)]
    (if (every? #(= (count %) 3) [a b c])
      (every? #(> *eps*(abs  %)) (cross* ba bc))
      (> *eps* (abs (cross*-k ba bc))))))

(defn corner-condition
  "Returns the type of corner at point `b`, given `a` and `c` endpoints.
  `:colinear` -> a b c form a line
  `:reflex`   -> CCW angle from ab to bc is > 180 and < 360
  `:convex`   -> CCW angle from ab to bc is < 180 and > 0"
  [a b c]
  (let [ba (v- a b)
        bc (v- c b)
        k (cross*-k ba bc)]
    (cond
      (> *eps* (abs k)) :colinear
      (< *eps* k) :reflex
      (> (- *eps*) k) :convex)))

;; https://youtu.be/hTJFcHutls8?t=1473
;; use k component from cross product to quickly check if vector
;; is on right or left of another vector
;; check each triangle edge vector against corner to pt vectors
(defn pt-inside?
  "`True` if point `pt` is inside the triangle formed by points `a`, `b`, and `c`."
  [[a b c] pt]
  (when-not (colinear? a b c)
    (let [ab (v- b a)
          bc (v- c b)
          ca (v- a c)
          apt (v- pt a)
          bpt (v- pt b)
          cpt (v- pt c)]
      (not
        (or (<= (cross*-k ab apt) 0)
            (<= (cross*-k bc bpt) 0)
            (<= (cross*-k ca cpt) 0))))))

(defn style
  "Merge a style map into the given element."
  [[k props & content] style-map]
  (into [k (merge props style-map)] content))

(defn centroid-of-pts
  "Calculates the arithmetic mean position of the given `pts`."
  [pts]
  (let [ndim (count (first (sort-by count pts)))
        splits (for [axis (range 0 ndim)]
                 (map #(nth % axis) pts))]
    (mapv #(apply average %) splits)))

(defn bounds-of-pts
  "Calculates the axis-aligned-bounding-box of `pts`."
  [pts]
  (when (seq pts)
    (let [xmax (apply max (map first pts))
          ymax (apply max (map second pts))
          xmin (apply min (map first pts))
          ymin (apply min (map second pts))]
      (vector [xmin ymin]
              [xmax ymin]
              [xmax ymax]
              [xmin ymax]))))

(defn bb-dims
  "Returns the dimensions of the bounding box defined by `pts`."
  [pts]
  (when-let [bounds (bounds-of-pts pts)]
    (let [[[xmin ymin] _ [xmax ymax] _] bounds]
      [(- xmax xmin) (- ymax ymin)])))

(defn offset-edge
  "Offset an edge defined by points `a` and `b` by distance `d` along the vector perpendicular to the edge."
  [[a b] d]
  (let [p (perpendicular (v- b a))
        pd (v* (normalize p) (repeat (- d)))
        xa (v+ a pd)
        xb (v+ b pd)]
    [xa xb]))

(defn- cycle-pairs
  "Creates pairs of points for line segments, including a segment from the last to the first point."
  [pts]
  (let [n (count pts)]
    (vec (take n (partition 2 1 (cycle pts))))))

(defn- wrap-list-once
  "Shifts a list by one to the right.
  [1 2 3] -> [3 1 2]"
  [s]
  (conj (drop-last s) (last s)))

(defn- every-other
  "Returns every even indexed element of the vector `v`."
  [v]
  (let [n (count v)]
    (map #(get v %) (filter even? (range n)))))

(defn offset-pts
  "Offset a polygon or polyline defined by points `pts` a distance of `d`. CCW point winding will result in an outward offset."
  [pts d]
  (let [edges (cycle-pairs pts)
        opts (mapcat #(offset-edge % d) edges)
        oedges (every-other (cycle-pairs opts))
        edge-pairs (cycle-pairs oedges)]
    (wrap-list-once (map #(apply line-intersection %) edge-pairs))))

(defn scale-pt-from-center
  "Scales a point [`x` `y`] by [`sx` `sy`] as if it were centered at [`cx` `cy`]."
  [[x y] [sx sy] [cx cy]]
  [(+ (* (- x cx) sx) cx)
   (+ (* (- y cy) sy) cy)])

;; easing functions are easier to understand with visuals:
;; https://easings.net/

(defn ease-in-sin
  "Remaps value `t`, which is assumed to be between 0 and 1.0, to a sin curve, affecting values closer to 1."
  [t]
  (- 1 (Math/cos (/ (* Math/PI t) 2))))

(defn ease-out-sin
  "Remaps value `t`, which is assumed to be between 0 and 1.0, to a sin curve, affecting values closer to 0."
  [t]
  (Math/sin (/ (* Math/PI t) 2)))

(defn ease-in-out-sin
  "Remaps value `t`, which is assumed to be between 0 and 1.0, to a sin curve."
  [t]
  (/ (- (Math/cos (* Math/PI t)) 1) -2))
