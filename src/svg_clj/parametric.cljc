(ns svg-clj.parametric
  "Provides functions that generate lists of points or return parametric curve functions."
  (:require [svg-clj.algorithms :as alg]
            [svg-clj.utils :as u]))

(defn check-parametric
  "Utility fn to help diagnose parametric function issues."
  [f]
  (let [fdata (try (f) (catch #?(:cljs :default :clj Exception) _))
        [f0 f05 f1] (map f [0 0.5 1])
        t0 (if (seqable? f0) f0 [f0])
        t05 (if (seqable? f05) f05 [f05])
        t1 (if (seqable? f1) f1 [f1])
        dim (count t05)
        required [:fn :input :vertex-params :length :origin]
        keys-pred (every? #(contains? fdata %) required)
        t0-pred (and t0 (= (count t0) dim) (every? number? t0))
        t1-pred (and t1 (= (count t1) dim) (every? number? t1))
        missing (when-not keys-pred (remove (set (keys fdata)) (set required)))
        result {:dimension dim
                :data fdata
                :valid-data keys-pred
                :valid-t0 t0-pred
                :valid-t1 t1-pred}]
    (cond-> result
      missing       (assoc-in [:error :missing] missing)
      (not fdata)   (assoc-in [:error :invalid-0-arity] fdata)
      (not t0-pred) (assoc-in [:error :invalid-t0] t0)
      (not t1-pred) (assoc-in [:error :invalid-t1] t1))))

(defn valid-parametric?
  "`True` if `f` is a properly built parametric function."
  [f]
  (nil? (:error (check-parametric f))))

(defn- remap-within
  "Shift the parameter range of `f` from 0 to 1 to `start` to `end`."
  [f [start end] x]
  (when (and (>= x start) (< x end))
    (let [step (- end start)
          t (/ (- x start) step)]
      (f t))))

(defn arc-length
  "Calculate the arc length of `curve`, being exact where possible and estimating otherwise.
  For example, bezier curves are estimated, but circles and arcs have exact results (barring rounding)."
  ([curve] (arc-length curve 0 1))
  ([curve t] (arc-length curve 0 t))
  ([curve ta tb]
   (let [seg 13500
         start (/ (* ta seg) seg)
         end   (/ (inc (* tb seg)) seg)]
     (->> (range start end (/ 1 seg))
          (map curve)
          (partition 2 1)
          (map #(apply u/distance %))
          (reduce +)
          (#(u/round % 5))))))

(defn regular-polygon-pts
  "Return a list of points making up a polygon with distance to the points `r` and `n` edges."
  [r n]
  (let [angle (* 2 Math/PI (/ 1 n))]
    (map #(vector (u/round (* r (Math/cos (* % angle))) 5)
                  (u/round (* r (Math/sin (* % angle))) 5))
         (range n))))

(defn rect-grid
  "Build a rectilinear grid with `nx`, `ny` points in x and y directions, with `x-spacing` and `y-spacing` between each point in the x and y directions respectively.
  Returned as a flat list of points from [0 0] in a 'Z' pattern to [(* nx x-spacing) (* ny y-spacing)]."
  [nx ny x-spacing y-spacing]
  (for [b (range ny)
        a (range nx)]
    [(* a x-spacing) (* b y-spacing)]))

;; todo: make this work. it's broken.
(defn hex-grid
  "Build a hexagonal grid. Doesn't work yet."
  [nx ny w]
  (let [a-offset (/ w 2)
        h (/ w 0.8660254)]
    (concat
     (for [x (range 0 nx)
           y (range 0 (Math/floor (/ ny 2)))]
       [(+ a-offset (* w x)) (* 2 h y)])
     (for [x (range 0 nx)
           y (range 1 (Math/ceil (/ ny 2)))]
       [(* w x) (+ h (* 2 h y))]))))

(defn line
  "Create a parametric function representing a straight line.
  The returned function takes a parameter `t` between 0 and 1, where t 0 = `a` and t 1 = `b`."
  [a b]
  (fn
    ([] {:fn `line
         :input [a b]
         :origin (u/centroid-of-pts [a b])
         :vertex-params [0 1]
         :length (u/distance a b)})
    ([t]
     (cond
       (= (float t) 0.0) a
       (= (float t) 1.0) b
       :else
       (u/v+ a (u/v* (u/v- b a) (repeat t)))))))

(defn fastline
  "Create a parametric function representing a straight line, with no checks and slightly faster implementation meant primarily for use in the bezier implementation.
  The returned function takes a parameter `t` between 0 and 1, where t 0 = `a` and t 1 = `b`."
  [[ax ay :as a] b]
  (let [[vx vy] (u/v- b a)]
    (fn [t]
      [(+ ax (* vx t))
       (+ ay (* vy t))])))

(defn polyline
  "Create a parametric function representing a polyline with straight segments defined by `pts`.
  The returned function takes a parameter `t` between 0 and 1, where t 0 = (first `pts`) and t 1 = (last `pts`)."
  [pts]
  (let [lines (map (partial apply line) (partition 2 1 pts))
        length (reduce + (map #(:length (%)) lines))
        intervals (->> lines
                       (map #(:length (%)))
                       (reductions +)
                       (concat [0])
                       (map #(/ % length))
                       (partition 2 1))]
    (fn
      ([] {:fn `polyline
           :input [pts]
           :origin (u/centroid-of-pts pts)
           :vertex-params (concat [0] (mapv second intervals))
           :length length})
      ([t]
       (cond
         (= (float t) 0.0) (first pts)
         (= (float t) 1.0) (last pts)
         :else
         (first
          (filter some?
                  (map #(remap-within %1 %2 t) lines intervals))))))))

(defn polygon
  "Create a parametric function representing a polygon with straight segments defined by `pts`.
  The returned function takes a parameter `t` between 0 and 1, where t 0 and 1 = (first `pts`)."
  [pts]
  (let [pts (concat (vec pts) [(first pts)])
        lines (map (partial apply line) (partition 2 1 pts))
        length (reduce + (map #(:length (%)) lines))
        intervals (->> lines
                       (map #(:length (%)))
                       (reductions +)
                       (concat [0])
                       (map #(/ % length))
                       (partition 2 1))]
    (fn
      ([] {:fn `polygon
           :input [pts]
           :origin (u/centroid-of-pts pts)
           :vertex-params (concat [0] (mapv second intervals))
           :length (reduce + (map #(:length (%)) lines))})
      ([t]
       (cond
         (= (float t) 0.0) (first pts)
         (= (float t) 1.0) (last pts)
         :else
         (first
          (filter some?
                  (map #(remap-within %1 %2 t) lines intervals))))))))

(defn- radius-from-pts
  "compute the radius of an arc defined by 3 points"
  [p1 p2 p3]
  (let [a (u/distance p3 p2)
        b (u/distance p3 p1)
        c (u/distance p2 p1)
        s (/ (+ a b c) 2)
        sa ( - s a)
        sb ( - s b)
        sc ( - s c)
        rt (Math/sqrt ^double (* s sa sb sc))
        radius (/ (/ (* a b c) 4) rt)]
    radius))

(defn- center-from-pts
  "compute the center point of an arc through 3 points"
  [p1 p2 p3]
  (let [u1 (u/v- p2 p1)
        u2 (u/v- p3 p1)
        w1 (u/cross* (u/v- p3 p1) u1)
        u (u/normalize u1)
        w (u/normalize w1)
        v (u/cross* w u)
        [bx _] [(u/dot* u1 u) 0]
        [cx cy] [(u/dot* u2 u) (u/dot* u2 v)]
        h (/ (+ (Math/pow (- cx (/ bx 2)) 2)
                (Math/pow cy 2)
                (- (Math/pow (/ bx 2) 2)))
             (* 2 cy))]
    (u/v+ p1
          (u/v* (repeat (/ bx 2)) u)
          (u/v* (repeat h) v))))

(defn circle
  "Create a parametric function representing a circle with radius `r` centered at the origin, or circumscribing points `a`, `b`, and `c`, as long as the three points are not colinear.
  The returned function takes a parameter `t` between 0 and 1, where t 0 and 1 = [r 0] or centroid + calcuated radius."
  ([r]
   (fn
     ([] {:fn `circle
          :input [r]
          :origin [0 0]
          :vertex-params [0]
          :length (* Math/PI 2 r)})
     ([t]
      (let [t (* 2 Math/PI t)
            x (* r (Math/cos t))
            y (* r (Math/sin t))]
        [x y]))))

  ([a b c]
   (when-not (u/colinear? a b c)
     (let [[a b c] (map #(conj % 0) [a b c])
           n (u/normalize (u/normal a b c))
           r (radius-from-pts a b c)
           cp (center-from-pts a b c)
           u (u/normalize (u/v- a cp))
           v (u/cross* n u)]
       (fn
         ([] {:fn `circle
              :input [a b c]
              :origin cp
              :vertex-params [0]
              :length (* Math/PI 2 r)
              :radius r})
         ([t]
          (cond
            (or (< t 0.0) (> t 1.0)) nil
            (= (float t) 0.0) (vec (drop-last a))
            (= (float t) 1.0) (vec (drop-last a))
            :else
            (let [t (* 2 Math/PI t)]
              (mapv
                #(u/round % 5)
                (drop-last
                  (u/v+ cp
                        (u/v* (repeat (* r (Math/cos t))) u)
                        (u/v* (repeat (* r (Math/sin t))) v))))))))))))

(defn arc
  "Create a parametric function representing an arc drawn from `a` through `b` and ending at `c`, as long as the three points are not colinear.
  The returned function takes a parameter `t` between 0 and 1, where t 0 = `a` and t 1 = `c`."
  [a b c]
  (when-not (u/colinear? a b c)
    (let [[a b c] (map #(conj % 0) [a b c])
          f (circle a b c)
          cp (center-from-pts a b c)
          angle (u/angle-from-pts a cp c)
          r (radius-from-pts a b c)]
      (fn
        ([] {:fn `arc
             :input [a b c]
             :origin cp
             :vertex-params [0 1]
             :length (* Math/PI 2 r (/ angle 360))
             :radius r
             :center cp})
        ([t]
         (let [t (* t (/ angle 360.0))]
           (f t)))))))

;; https://www.mathsisfun.com/geometry/ellipse-perimeter.html
;; uses 'Infinite Series 2' exact calc. using 4 terms.
(defn- ellipse-perimeter
  "Estimate the perimeter of an ellipse with radii `rx` and `ry`."
  [rx ry]
  (let [h (/ (Math/pow (- rx ry) 2)
             (Math/pow (+ rx ry) 2))]
    (* Math/PI (+ rx ry)
       (+ 1
          (* h (/ 1 4))
          (* h h (/ 1 64))
          (* h h h (/ 1 256))))))

(defn ellipse
  "Create a parametric function representing an ellipse with radii `rx`, `ry` centered at the origin.
  The returned function takes a parameter `t` between 0 and 1, where t 0 and 1 = [rx 0]."
  [rx ry]
  (fn
    ([] {:fn `ellipse
         :input [rx ry]
         :origin [0 0]
         :vertex-params [0]
         :length (ellipse-perimeter rx ry)})
    ([t]
     (let [t (* 2 Math/PI t)
           x (* rx (Math/cos t))
           y (* ry (Math/sin t))]
       [x y]))))

(defn- quadratic-bezier
  [a b c]
  (fn [t]
    (let [l1 (fastline a b)
          l2 (fastline b c)
          l3 (fastline (l1 t) (l2 t))]
      (l3 t))))

(defn- bezier*
  [pts]
  (if (= 3 (count pts))
    (apply quadratic-bezier pts)
    (let [lines (map #(apply fastline %) (partition 2 1 pts))]
      (fn
        [t]
        (let [npts (map #(% t) lines)]
          ((bezier* npts) t))))))

(defn bezier
  "Create a parametric function representing a bezier curve with control points `pts`, as long as there are at least 3 points.
  The returned function takes a parameter `t` between 0 and 1, where t 0 = (first `pts`) and t 1 = (last `pts`)."
  [pts]
  (when (> (count pts) 2)
    (let [curve (bezier* pts)
          length (arc-length curve)]
      (fn
        ([] {:fn `bezier
             :input [pts]
             :origin (u/centroid-of-pts pts)
             :vertex-params [0 1]
             :length length})
        ([t] (curve t))))))

(defn- next-pascal
  [row]
  (vec (concat [(first row)]
          (mapv #(apply + %) (partition 2 1 row))
          [(last row)])))

(defn- binomial
  [n i]
  (let [pascal-tri-row (last (take (inc n) (iterate next-pascal [1])))]
  (get pascal-tri-row i)))

(defn- polynomial
  [n i t]
  (* (Math/pow (- 1 t) (- n i)) (Math/pow t i)))

(defn- half-bezier
  [ws t]
  (let [n (dec (count ws))
        poly (partial polynomial n)
        bi (partial binomial n)]
    (reduce + (map-indexed
               (fn [i w]
                 (* (bi i) (poly i t) w))
               ws))))

(defn- rational-bezier*
  [pts wts]
  (let [xs (map #(* (first %1) %2) pts wts)
        ys (map #(* (second %1) %2) pts wts)
        dn (partial half-bezier wts)]
    (fn [t]
      [(/ (half-bezier xs t) (dn t))
       (/ (half-bezier ys t) (dn t))])))

;; todo: write tests to see if this actually works properly.
(defn rational-bezier
  "Create a parametric function representing a rational bezier curve with control points `pts` and weights `wts`, as long as there are at least 3 points and 3 wts.
  The returned function takes a parameter `t` between 0 and 1, where t 0 = (first `pts`) and t 1 = (last `pts`)."
  [pts wts]
  (let [curve (rational-bezier* pts wts)
        length (arc-length curve)]
    (fn
      ([] {:fn `rational-bezier
           :input [pts wts]
           :origin (u/centroid-of-pts pts)
           :vertex-params [0 1]
           :length length})
      ([t] (curve t)))))

(defn piecewise-curve
  "Create a parametric function representing compound curve composed of `curves`.
  The returned function takes a parameter `t` between 0 and 1, where t 0 = the start of the first curve  and t 1 = the end of the last curve."
  [curves]
  (let [step (/ 1.0 (count curves))
        intervals (partition 2 1 (range 0 (+ 1 step) step))
        remapf (fn [curve [start end]]
                 (let [vertex-params (:vertex-params (curve))
                       sc (- end start)]
                   (map #(+ start (* sc %)) vertex-params)))
        vertex-params (vec (distinct (mapcat remapf curves intervals)))
        origin (u/centroid-of-pts (map #(:origin (%)) curves))
        length (reduce + (map #(:length (%)) curves))
        sample-curve (first curves)]
    (fn
      ([] {:fn `piecewise-curve
           :input [curves]
           :origin origin
           :dimension (count (sample-curve 0.5))
           :vertex-params vertex-params
           :length length})
      ([t]
       (cond
         (= (float t) 0.0) ((first curves) 0)
         (= (float t) 1.0) ((last curves) 1)
         :else
         (first
          (filter some?
                  (map #(remap-within %1 %2 t) curves intervals))))))))

(defn split-bezier
  "Returns the Control Point 'de Casteljau Skeleton', used to derive split Bezier Curve Control Points."
  ([curve t]
   (let [pts (-> (curve) :input first)]
     (split-bezier {:a [(first pts)]
                    :b [(last pts)]} pts t)))

  ([{:keys [a b]} pts t]
   (let [cs (map #(apply line  %) (partition 2 1 pts))
         npts (map #(% t) cs)]
     (if (= 1 (count npts))
       {:a (conj a (first npts))
        :b (-> b
               reverse
               (conj (first npts))
               vec)}
       (recur {:a (conj a (first npts))
               :b (conj b (last npts))} npts t)))))

(defn- get-t
  "Estimate curve parameter `t` that corresponds to length-percentage `target-lp`."
  [curve target-lp]
  (let [eps 0.00001
        length (:length (curve))
        target-l (* length target-lp)]
    (loop [t target-lp
           n 0]
        (let [next-t (+ t (/ (- target-l (arc-length curve t)) target-l))]
          (if (or
               (= (u/round t 4) (u/round next-t 4))
               (< (abs (- target-l (arc-length curve t))) eps)
               (< 300 n))
            next-t
            (recur next-t (inc n)))))))

(defn- get-t-at-distance
  [curve d]
  (let [target-lp (/ d (:length (curve)))]
    (get-t curve target-lp)))

(defn split-bezier-between
  "Split the given bezier curve `curve` between parameters `ta` and `tb`, where each is between 0 and 1."
  [curve ta tb]
  (let [da (arc-length curve ta)
        split1 (split-bezier curve tb)
        curve1 (bezier (:a split1))
        partial-result {:c (:b split1)}
        ta1 (get-t-at-distance curve1 da)]
    (merge (split-bezier curve1 ta1) partial-result)))

(defn multi-split-bezier
  "Split the given bezier curve `curve` at each parameter in `ts`, where each is between 0 and 1."
  ([curve ts]
   (let [ds (map #(arc-length curve %) (sort ts))]
     (multi-split-bezier [] curve (reverse ds))))
  ([acc curve ds]
   (if (< 1 (count ds))
     (let [remapped-t (get-t-at-distance curve (first ds))
           {:keys [a b]} (split-bezier curve remapped-t)]
       (recur (conj acc b) (bezier a) (rest ds)))
     (let [remapped-t (get-t-at-distance curve (first ds))
           {:keys [a b]} (split-bezier curve remapped-t)]
       (-> acc
           (conj b)
           (conj a)
           reverse)))))

(defn uniform-split-bezier
  "Split the given bezier curve `curve` evenly into `n` segments."
  [curve n-segments]
  (let [l (arc-length curve)
        step (/ l n-segments)
        ds (range step l step)]
    (if (= 2 n-segments)
      (split-bezier curve 0.5)
      (multi-split-bezier [] curve (reverse (sort ds))))))

;; todo: test/fix all bezier splitting functions

#_(def test-spline
  (let [degree 3
        pts [[0 0] [5 5] [10 -5] [15 25] [20 -5] [25 5] [30 0]]
        knots [1 2 3 4 5 6 7 8 9 10 11]]
    (partial b-spline-inner [pts degree knots] [1 pts])))

(defn sinwave
  "Creates a parametric function of a sinwave with amplitude `amp`, and frequency `freq`."
  [amp freq]
  (fn [t]
    (* amp (Math/sin (* t freq Math/PI)))))

(defn blend
  "Buildsa a parametric function that is a blend of parametric functions `fa` `fb` by `alpha` between 0 and 1 where 0 is 100% `fa`, and 1 is 100% `fb`.
  Optionally pass an `easefn` to apply to the alpha."
  ([fa fb alpha]
   (fn [t]
     (let [line (line (fa t) (fb t))]
       (line alpha))))
  ([fa fb easefn alpha]
   (fn [t]
     (let [line (line (fa t) (fb t))]
       (line (easefn alpha))))))

(defn eased-polyline
  "Create a parametric function which is a smoothed polyline using an easing function `easefn`."
  [pts easefn]
  (let [lines (map (partial apply line) (partition 2 1 pts))
        length (reduce + (map #(:length (%)) lines))
        intervals (->> lines
                       (map #(:length (%)))
                       (reductions +)
                       (concat [0])
                       (map #(/ % length))
                       (partition 2 1))
        easedlines (map #(fn [t] (% (easefn t))) lines)]
    (fn
      ([] {:fn `eased-polyline
           :input [pts easefn]
           :length length})
      ([t]
       (cond
         (= (float t) 0.0) (first pts)
         (= (float t) 1.0) (last pts)
         :else
         (first
          (filter some?
                  (map #(remap-within %1 %2 t) easedlines intervals))))))))

;; todo: turn this into a proper parametric fn (add 0-arity, test, etc.)
(defn multiblend
  "Blend multiple parametric curves `fs` together with `alpha` and an optional `easefn`."
  ([fs alpha]
   (fn [t]
     (let [line (polyline (map #(% t) fs))]
       (line alpha))))
  ([fs easefn alpha]
   (fn [t]
     (let [line (eased-polyline (map #(% t) fs) easefn)]
       (line alpha)))))

;; todo: turn this into a proper parametric fn (add 0-arity, test, etc.)
(defn fn-offset
  "Offset the parametric curve `curve` with an offsetting function `f`, which modfies the paramter t in some way."
  [curve f]
  (let [eps u/*eps*]
    (fn [t]
      (let [t (cond (<= (- 1 eps) t) (- 1 eps)
                    (> eps t) eps
                    :else t)
            n (u/normalize (u/normal (curve (- t eps)) (curve (+ t eps))))
            tpt (curve t)
            l (line tpt (u/v+ tpt n))]
        (l (f t))))))

(defn shift-pts
  "Shift a list of `pts` to begin at `start`, preserving order and cycling the list.
If no `start` is provided, pt with lowest x and y values is used."
  ([pts]
   (let [start (first (sort-by (juxt first second) pts))]
     (shift-pts pts start)))
  ([pts start]
   (let [[back front] (split-with (complement #{start}) pts)]
     (concat front back))))

(defn simplify
  "Simplifies the list of `pts` by evenly stepping `n` times along the parametric curve produced by the original list.
This does not guarantee that input pts are preserved in the output."
  [pts n]
  (let [c (polygon pts)]
    (mapv #(c (/ % (inc n))) (range n))))

(defn stroke-pts
  "Return a list of points that define a constant `width` stroke with `n-segments` along the `curve`."
  [curve width n-segments]
  (let [tcurve (fn-offset curve #(* 0.5 width))
        bcurve (fn-offset curve #(* -0.5 width))]
    (concat [(curve 0)]
            (map #(tcurve (/ % n-segments)) (range (inc n-segments)))
            [(curve 1)]
            (map #(bcurve (/ % n-segments)) (reverse (range (inc n-segments)))))))

;; todo: rework this implementation to remove need for 'pline'
#_(defn- pline
  [line]
  (let [[_ {:keys [x1 y1 x2 y2]}] line]
    (line [x1 y1] [x2 y2])))

#_(defn tapered-stroke-pts
  "Return a list of points that define a maximum `width` stroke with `n-segments` along the `curve`, where the points are tapered from both ends up to the parameter `taper-t` on the curve.
  The max. `taper-t` can be is 0.5 to have the stroke taper up to maximum width at the center of the curve down again to the end of the curve."
  [curve width n-segments taper-t]
  (let [taper-n (int (* n-segments taper-t))
        taper (map #(u/ease-out-sin (/ % taper-n)) (range taper-n))
        dist (concat taper (repeat (- n-segments (* 2 (count taper))) 1) (reverse taper))
        tlns (->> (el/line [0 0] [0 (* 0.5 width)])
                  (repeat (inc n-segments))
                  (#(lo/distribute-on-curve % curve))
                  (map pline))
        blns (->> (el/line [0 0] [0 (* -0.5 width)])
                  (repeat n-segments)
                  (#(lo/distribute-on-curve % curve))
                  (map pline))]
    (concat [(curve 0)]
            (map #(%1 (* 1 (- 1 %2))) tlns dist)
            [(curve 1)]
            (reverse (map #(%1 (* 1 (- 1 %2))) blns dist)))))

(defn fillet-pts
  "Return a list of points defining an approximation of the shaped defined by `pts` with fillets of radius `r` at all of the corners."
  [pts r]
  (let [fillet (regular-polygon-pts r 50)
        ipts (u/offset-pts pts (- r))
        f (fn [pt] (map #(u/v+ pt %) fillet))
        npts (mapcat f ipts)]
    (alg/hull npts)))

(defn chamfer-pts
  "Return a list of points defining a new shape derived from the shape made by `pts` with chamfers of radius `r` at all of the corners."
  [pts r]
  (let [fillet (regular-polygon-pts r 50)
        ipts (u/offset-pts pts (- r))
        f (fn [pt] (map #(u/v+ pt %) fillet))
        npts (mapcat f ipts)]
    (->> (alg/hull npts)
         (partition 2 1)
         (sort-by #(apply u/distance %))
         reverse
         (take (count pts))
         (apply concat)
         alg/hull)))

(defn translate
  "Translate the parametric function `f` by [`x` `y`]."
  [f [x y]]
  (let [data (f)]
    (fn
      ([] (merge data
                 {:fn `translate
                  :origin (u/v+ (:origin data) [x y])
                  :input [f [x y]]}))
      ([t]
       (u/v+ (f t) [x y])))))

(defn rotate
  "Rotate the parametric function `f` by `deg` around its origin."
  [f deg]
  (let [data (f)
        ctr (:origin data)]
    (fn
      ([] {:fn `rotate
           :input [f deg]})
      ([t]
       (u/rotate-pt-around-center (f t) deg ctr)))))

(defn scale
  "Scale the parametric function `f` by [`sx` `sy`] around its origin."
  [f [sx sy]]
  (let [data (f)
        ctr (:origin data)]
    (fn
      ([] (merge data
                 {:fn `scale
                  :input [f [sx sy]]}))
      ([t]
       (u/scale-pt-from-center (f t) [sx sy] ctr)))))
