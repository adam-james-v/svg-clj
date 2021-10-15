(ns svg-clj.parametric
  "Provides functions that generate lists of points or return parametric curve functions, intended for use with layout functionality."
  (:require [svg-clj.utils :as utils]))

(defn arc-length
  ([curve] (arc-length curve 0 1))
  ([curve t] (arc-length curve 0 t))
  ([curve ta tb]
   (let [delta 0.0000075]
     (->> (range ta (+ tb delta) delta)
          (map curve)
          (partition 2 1)
          (map #(apply utils/distance %))
          (reduce +)))))

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
         :length (utils/distance a b)})
    ([t]
     (cond
       (= (float t) 0.0) a
       (= (float t) 1.0) b
       :else
       (utils/v+ a (utils/v* (utils/v- b a) (repeat t)))))))

(defn- fastline
  [[ax ay :as a] [bx by :as b]]
  (let [[vx vy] (utils/v- b a)]
    (fn [t]
      [(+ ax (* vx t))
       (+ ay (* vy t))])))

(defn- remap-within
  [f [start end] x]
  (when (and (>= x start) (< x end))
    (let [step (- end start)
          t (/ (- x start) step)]
      (f t))))

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
        [bx _] [(utils/dot* u1 u) 0]
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
        d (Math/abs ^double (* l1 l2))]
    (when (not (= 0.0 (float d)))
      (utils/to-deg (Math/acos (/ n d))))))

(defn circle
  ([r]
   (fn
     ([] {:fn `circle
          :input [r]
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
            :length (* Math/PI 2 r)})
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
         :length (ellipse-perimeter rx ry)})
    ([t]
     (let [t (* 2 Math/PI t)
           x (* rx (Math/cos t))
           y (* ry (Math/sin t))]
       [x y]))))

(defn arc
  [a b c]
  (let [[a b c] (map #(conj % 0) [a b c])
        f (circle a b c)
        cp (center-from-pts a b c)
        angle (angle-from-pts a cp c)
        r (radius-from-pts a b c)]
    (fn
      ([] {:fn `arc
           :input [a b c]
           :length (* Math/PI 2 r (/ angle 360))})
      ([t]
       (let [t (* t (/ angle 360.0))]
         (f t))))))

(defn- quadratic-bezier
  [a b c]
  (fn [t]
    (let [l1 (line a b)
          l2 (line b c)
          l3 (line (l1 t) (l2 t))]
      (l3 t))))

(defn bezier
  [pts]
  (if (= 3 (count pts))
    (apply quadratic-bezier pts)
    (let [lines (map #(apply line %) (partition 2 1 pts))] 
      (fn
        ([] {:fn `bezier
             :input [pts]})
        ([t]
         (let [npts (map #(% t) lines)]
           ((bezier npts) t)))))))

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

(defn- t-from-curve-distance
  [curve d]
  (let [eps 0.000001
        l (arc-length curve)
        guess (/ d l)
        itr (fn [[#_t-prev _ t-guess]]
              (let [d-guess (arc-length curve t-guess)]
                [t-guess (+ t-guess (/ (- d d-guess) l))]))]
    (->> (iterate itr [0 guess])
         (take 25)
         (take-while #(< eps (Math/abs (apply - %))))
         last
         last
         (#(utils/round % 5)))))

(defn split-bezier-between
  [curve ta tb]
  (let [da (arc-length curve ta)
        split1 (split-bezier curve tb)
        curve1 (bezier (:a split1))
        partial-result {:c (:b split1)}
        ta1 (t-from-curve-distance curve1 da)]
    (merge (split-bezier curve1 ta1) partial-result)))

(defn multi-split-bezier
  ([curve ts]
   (let [ds (map #(arc-length curve %) (sort ts))]
     (multi-split-bezier [] curve (reverse ds))))
  ([acc curve ds]
   (if (< 1 (count ds))
     (let [remapped-t (t-from-curve-distance curve (first ds)) 
           {:keys [a b]} (split-bezier curve remapped-t)]
       (recur (conj acc b) (bezier a) (rest ds)))
     (let [remapped-t (t-from-curve-distance curve (first ds)) 
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

(defn rational-bezier
  [pts wts]
  (let [xs (map #(* (first %1) %2) pts wts)
        ys (map #(* (second %1) %2) pts wts)
        dn (partial half-bezier wts)]
    (fn [t]
      [(/ (half-bezier xs t) (dn t)) 
       (/ (half-bezier ys t) (dn t))])))

#_(def test-spline
  (let [degree 3
        pts [[0 0] [5 5] [10 -5] [15 25] [20 -5] [25 5] [30 0]]
        knots [1 2 3 4 5 6 7 8 9 10 11]]
    (partial b-spline-inner [pts degree knots] [1 pts])))

(defn translate
  [f [x y]]
  (fn
    ([] {:fn `translate
         :input [f [x y]]})
    ([t]
     (utils/v+ (f t) [x y]))))

(defn rotate
  [f deg]
  (let [ctr (utils/centroid-of-pts (map f (range 0 1 0.05)))]
    (fn
      ([] {:fn `rotate
           :input [f deg]})
      ([t]
       (utils/rotate-pt-around-center (f t) deg ctr)))))

(defn scale
  [f [sx sy]]
  (let [ctr (utils/centroid-of-pts (map f (range 0 1 0.05)))]
    (fn
      ([] {:fn `scale
           :input [f [sx sy]]})
      ([t]
       (utils/scale-pt-from-center (f t) [sx sy] ctr)))))
