(ns svg-clj.parametric
  "Provides functions that generate lists of points or return parametric curve functions."
  (:require [svg-clj.utils :as utils]
            [svg-clj.algorithms :as alg]))

(defn check-parametric
  [f]
  (let [fdata (try (f) (catch #?(:cljs :default :clj Exception) e))
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
  [f]
  (nil? (:error (check-parametric f))))

(defn- remap-within
  [f [start end] x]
  (when (and (>= x start) (< x end))
    (let [step (- end start)
          t (/ (- x start) step)]
      (f t))))

(defn arc-length
  ([curve] (arc-length curve 0 1))
  ([curve t] (arc-length curve 0 t))
  ([curve ta tb]
   (let [seg 13500
         start (/ (* ta seg) seg)
         end   (/ (inc (* tb seg)) seg)]
     (->> (range start end (/ 1 seg))
          (map curve)
          (partition 2 1)
          (map #(apply utils/distance %))
          (reduce +)
          (#(utils/round % 5))))))

(defn regular-polygon-pts
  [r n]
  (let [angle (* 2 Math/PI (/ 1 n))]
    (map #(vector (utils/round (* r (Math/cos (* % angle))) 5)
                  (utils/round (* r (Math/sin (* % angle))) 5))
         (range n))))

(defn rect-grid
  [nx ny x-spacing y-spacing]
  (for [b (range ny)
        a (range nx)]
    [(* a x-spacing) (* b y-spacing)]))

(defn hex-grid
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
  [a b]
  (fn
    ([] {:fn `line
         :input [a b]
         :origin (utils/centroid-of-pts [a b])
         :vertex-params [0 1]
         :length (utils/distance a b)})
    ([t]
     (cond
       (= (float t) 0.0) a
       (= (float t) 1.0) b
       :else
       (utils/v+ a (utils/v* (utils/v- b a) (repeat t)))))))

(defn fastline
  [[ax ay :as a] [bx by :as b]]
  (let [[vx vy] (utils/v- b a)]
    (fn [t]
      [(+ ax (* vx t))
       (+ ay (* vy t))])))

(defn polyline
  [pts]
  (let [step (/ 1.0 (dec (count pts)))
        lines (map (partial apply line) (partition 2 1 pts))
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
           :origin (utils/centroid-of-pts pts)
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
  [pts]
  (let [pts (concat (vec pts) [(first pts)])
        step (/ 1.0 (dec (count pts)))
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
           :origin (utils/centroid-of-pts pts)
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
  (let [a (utils/distance p3 p2)
        b (utils/distance p3 p1)
        c (utils/distance p2 p1)
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
  (let [u1 (utils/v- p2 p1)
        u2 (utils/v- p3 p1)
        w1 (utils/cross* (utils/v- p3 p1) u1)
        u (utils/normalize u1)
        w (utils/normalize w1)
        v (utils/cross* w u)
        [bx _] [(utils/dot* u1 u) 0]
        [cx cy] [(utils/dot* u2 u) (utils/dot* u2 v)]
        h (/ (+ (Math/pow (- cx (/ bx 2)) 2) 
                (Math/pow cy 2)
                (- (Math/pow (/ bx 2) 2))) 
             (* 2 cy))]
    (utils/v+ p1
              (utils/v* (repeat (/ bx 2)) u) 
              (utils/v* (repeat h) v))))

(defn circle
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
   (let [[a b c] (map #(conj % 0) [a b c])
         n (utils/normalize (utils/normal a b c))
         r (radius-from-pts a b c)
         cp (center-from-pts a b c)
         u (utils/normalize (utils/v- a cp))
         v (utils/cross* n u)]
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
            #(utils/round % 5)
            (drop-last 
             (utils/v+ cp
                       (utils/v* (repeat (* r (Math/cos t))) u)
                       (utils/v* (repeat (* r (Math/sin t))) v)))))))))))

(defn arc
  [a b c]
  (let [[a b c] (map #(conj % 0) [a b c])
        f (circle a b c)
        cp (center-from-pts a b c)
        angle (utils/angle-from-pts a cp c)
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
         (f t))))))

;; https://www.mathsisfun.com/geometry/ellipse-perimeter.html
;; uses 'Infinite Series 2' exact calc. using 4 terms.
(defn- ellipse-perimeter
  [rx ry]
  (let [h (/ (Math/pow (- rx ry) 2)
             (Math/pow (+ rx ry) 2))]
    (* Math/PI (+ rx ry)
       (+ 1
          (* h (/ 1 4))
          (* h h (/ 1 64))
          (* h h h (/ 1 256))))))

(defn ellipse
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
  [pts]
  (let [curve (bezier* pts)
        length (arc-length curve)]
    (fn
      ([] {:fn `bezier
           :input [pts]
           :origin (utils/centroid-of-pts pts)
           :vertex-params [0 1]
           :length length})
      ([t] (curve t)))))

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

(defn rational-bezier*
  [pts wts]
  (let [xs (map #(* (first %1) %2) pts wts)
        ys (map #(* (second %1) %2) pts wts)
        dn (partial half-bezier wts)]
    (fn [t]
      [(/ (half-bezier xs t) (dn t)) 
       (/ (half-bezier ys t) (dn t))])))

(defn rational-bezier
  [pts wts]
  (let [curve (rational-bezier* pts wts)
        length (arc-length curve)]
    (fn
      ([] {:fn `rational-bezier
           :input [pts wts]
           :origin (utils/centroid-of-pts pts)
           :vertex-params [0 1]
           :length length})
      ([t] (curve t)))))

(defn piecewise-curve
  [curves]
  (let [step (/ 1.0 (count curves))
        intervals (partition 2 1 (range 0 (+ 1 step) step))
        remapf (fn [curve [start end]]
                 (let [vertex-params (:vertex-params (curve))
                       sc (- end start)]
                   (map #(+ start (* sc %)) vertex-params)))
        vertex-params (vec (distinct (mapcat remapf curves intervals)))
        origin (utils/centroid-of-pts (map #(:origin (%)) curves))
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
               (= (utils/round t 4) (utils/round next-t 4))
               (< (utils/abs (- target-l (arc-length curve t))) eps)
               (< 300 n))
            next-t
            (recur next-t (inc n)))))))

(defn- get-t-at-distance
  [curve d]
  (let [target-lp (/ d (:length (curve)))]
    (get-t curve target-lp)))

(defn split-bezier-between
  [curve ta tb]
  (let [da (arc-length curve ta)
        split1 (split-bezier curve tb)
        curve1 (bezier (:a split1))
        partial-result {:c (:b split1)}
        ta1 (get-t-at-distance curve1 da)]
    (merge (split-bezier curve1 ta1) partial-result)))

(defn multi-split-bezier
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
  [curve n-segments]
  (let [l (arc-length curve)
        step (/ l n-segments)
        ds (range step l step)]
    (if (= 2 n-segments)
      (split-bezier curve 0.5)
      (multi-split-bezier [] curve (reverse (sort ds))))))

#_(def test-spline
  (let [degree 3
        pts [[0 0] [5 5] [10 -5] [15 25] [20 -5] [25 5] [30 0]]
        knots [1 2 3 4 5 6 7 8 9 10 11]]
    (partial b-spline-inner [pts degree knots] [1 pts])))

(defn sinwave
  [amp freq]
  (fn [t]
    (* amp (Math/sin (* t freq Math/PI)))))

(defn blend
  ([fa fb alpha]
   (fn [t]
     (let [line (line (fa t) (fb t))]
       (line alpha))))
  ([fa fb easefn alpha]
   (fn [t]
     (let [line (line (fa t) (fb t))]
       (line (easefn alpha))))))

(defn eased-polyline
  [pts easefn]
  (let [step (/ 1.0 (dec (count pts)))
        lines (map (partial apply line) (partition 2 1 pts))
        length (reduce + (map #(:length (%)) lines))
        intervals (->> lines
                       (map #(:length (%)))
                       (reductions +)
                       (concat [0])
                       (map #(/ % length))
                       (partition 2 1))
        easedlines (map #(fn [t] (% (easefn t))) lines)]
    (fn
      ([] {:fn `polyline
           :input [pts]
           :length length})
      ([t]
       (cond
         (= (float t) 0.0) (first pts)
         (= (float t) 1.0) (last pts)
         :else
         (first
          (filter some?
                  (map #(remap-within %1 %2 t) easedlines intervals))))))))

(defn multiblend
  ([fs alpha]
   (fn [t]
     (let [line (polyline (map #(% t) fs))]
       (line alpha))))
  ([fs easefn alpha]
   (fn [t]
     (let [line (eased-polyline (map #(% t) fs) easefn)]
       (line alpha)))))

(defn fn-offset
  [curve f]
  (let [eps 0.000001]
    (fn [t]
      (let [t (cond (<= (- 1 eps) t) (- 1 eps)
                    (> eps t) eps
                    :else t)
            n (utils/normalize (utils/normal (curve (- t eps)) (curve (+ t eps))))
            tpt (curve t)
            l (line tpt (utils/v+ tpt n))]
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

;; not certain if this is a great design idea yet
(defn remove-colinears [pts] (alg/remove-colinears pts))

(defn- pline
  [line]
  (let [[_ {:keys [x1 y1 x2 y2]}] line]
    (line [x1 y1] [x2 y2])))

(defn stroke-pts
  [curve width n-segments]
  (let [of (fn [_] (* 0.5 width))
        tcurve (fn-offset curve #(* 0.5 width))
        bcurve (fn-offset curve #(* -0.5 width))]
    (concat [(curve 0)]
            (map #(tcurve (/ % n-segments)) (range (inc n-segments)))
            [(curve 1)]
            (map #(bcurve (/ % n-segments)) (reverse (range (inc n-segments)))))))

#_(defn tapered-stroke-pts
  [curve width n-segments taper-t]
  (let [taper-n (int (* n-segments taper-t))
        taper (map #(ease-out-sin (/ % taper-n)) (range taper-n))
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
  [pts r]
  (let [fillet (regular-polygon-pts r 50)
        ipts (utils/offset-pts pts (- r))
        f (fn [pt] (map #(utils/v+ pt %) fillet))
        npts (mapcat f ipts)]
    (alg/hull npts)))

(defn chamfer-pts
  [pts r]
  (let [fillet (regular-polygon-pts r 50)
        ipts (utils/offset-pts pts (- r))
        f (fn [pt] (map #(utils/v+ pt %) fillet))
        npts (mapcat f ipts)]
    (->> (alg/hull npts)
         (partition 2 1)
         (sort-by #(apply utils/distance %))
         reverse
         (take (count pts))
         (apply concat)
         alg/hull)))

(defn translate
  [f [x y]]
  (let [data (f)]
    (fn
      ([] (merge data
                 {:fn `translate
                  :origin (utils/v+ (:origin data) [x y])
                  :input [f [x y]]}))
      ([t]
       (utils/v+ (f t) [x y])))))

(defn rotate
  [f deg]
  (let [data (f)
        ctr (:origin data)]
    (fn
      ([] {:fn `rotate
           :input [f deg]})
      ([t]
       (utils/rotate-pt-around-center (f t) deg ctr)))))

(defn scale
  [f [sx sy]]
  (let [data (f)
        ctr (:origin data)]
    (fn
      ([] (merge data
                 {:fn `scale
                  :input [f [sx sy]]}))
      ([t]
       (utils/scale-pt-from-center (f t) [sx sy] ctr)))))
