(ns svg-clj.parametric
  (:require [clojure.string :as str]
            [svg-clj.utils :as utils]))

(defn arc-length
  ([curve] (arc-length curve 0 1))
  ([curve t] (arc-length curve 0 t))
  ([curve ta tb]
   (let [eps 0.0000075]
     (->> (range ta (+ tb eps) eps)
          (pmap curve)
          (partition 2 1)
          (map #(apply utils/distance %))
          (reduce +)))))

;; https://pomax.github.io/bezierinfo/legendre-gauss.html
(def gaussian-quadrature-n-8
  [{:w 0.3626837833783620 :x -0.1834346424956498}
   {:w 0.3626837833783620 :x 0.1834346424956498}
   {:w 0.3137066458778873 :x -0.5255324099163290}
   {:w 0.3137066458778873 :x 0.5255324099163290}
   {:w 0.2223810344533745 :x -0.7966664774136267}
   {:w 0.2223810344533745 :x 0.7966664774136267}
   {:w 0.1012285362903763 :x -0.9602898564975363}
   {:w 0.1012285362903763 :x 0.9602898564975363}])

;; https://pomax.github.io/bezierinfo/#arclength
(defn arc-length-exact
  [curve]
  (let [lut gaussian-quadrature-n-8
        z 1
        z-frac (/ z 2)
        f (fn [t] (Math/sqrt (reduce + (map #(* % %) (curve t)))))
        len (fn [{:keys [w x]}]
              (* w (f (+ z-frac (* z-frac x)))))]
    (reduce + (map len lut))))

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
    ([] {:fn 'line :input [a b]})
    ([t]
     (cond
       (= (float t) 0.0) a
       (= (float t) 1.0) b
       :else
       (utils/v+ a (utils/v* (utils/v- b a) (repeat t)))))))

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
        d (Math/abs ^double (* l1 l2))]
    (when (not (= 0.0 (float d)))
      (utils/to-deg (Math/acos (/ n d))))))

(defn circle
  ([r]
   (fn
     ([] {:fn 'circle :input [r]})
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
       ([] {:fn 'circle :input [a b c]})
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

(defn ellipse
  [rx ry]
  (fn 
    ([] {:fn 'ellipse :input [rx ry]})
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
        angle (angle-from-pts a cp c)]
    (fn
      ([] {:fn 'arc :input [a b c]})
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
        ([] {:fn 'bezier :input [pts]})
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
        itr (fn [[t-prev t-guess]]
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

(defn- domain
  [degree knots]
  (let [knots (vec knots)]
    [(get knots degree)
     (get knots (- (count knots) 1 degree))]))

(defn- remap-t
  [degree knots t]
  (let [[ds de] (domain degree knots)
        sc (- de ds)]
    (+ ds (* sc t))))

(defn- section-index
  [degree knots t]
  (let [t (remap-t degree knots t)
        knots (vec knots)
        [ds de] (domain degree knots)]
    (->>
     (map (fn [[s0 s1]]
            (when (<= s0 t s1) s0))
                  (partition 2 1 (range ds (inc de))))
     (filter some?)
     first)))

;; degree example: quadratic b-spline = degree 2, cubic degree 3
;; order is (inc degree) and is the number of knots needed for any one section
;; order, k, is called 'knot interval'
;; n is number of control points. 
;; NOTE: for b-splines, the degree and n_cpts are NOT related.


;; current issue is index out of bounds. Seems like it's probably related to
;; incorrect s or i value, so (get knots i...) fails in some cases. 
(defn- b-spline-inner
  [[pts degree knots]
   [l v]
   t]
  (let [s (section-index degree knots t)
        order (inc degree)]
     (loop [v v
           i s]
      (if (> i (+ s l (- order)))
        (let [[x y :as vi] (get v i)
              numerator (- t (get knots i))
              denominator (- (get knots (+ i (- l) order))
                             (get knots i))
              alpha (/ numerator denominator)
              new-vi (utils/v+ (map #(* alpha %) vi)
                               (map #(* (- 1 alpha) %) (get v (dec i))))
              new-v (assoc v i new-vi)]
          (recur new-vi (dec i)))
        v))))

(defn b-spline
  [pts degree knots]
  (let [k (count knots)
        d degree
        order (inc degree)
        n (count pts)]
    (when (= k (+ d n 1))
      (fn [t]
        (let [s (section-index degree knots t)]
          (loop [v-outer pts
                 l 1]
            (if (<= l order)
              (let [new-v (b-spline-inner
                           [pts degree knots]
                           [l v-outer]
                           t)]
                (recur new-v (inc l))
                #_(loop [v v-outer
                         i s]
                    (if (> i (+ s l (- order)))
                      (let [[x y :as vi] (get v i)
                            numerator (- t (get knots i))
                            denominator (- (get knots (+ i (- l) order))
                                           (get knots i))
                            alpha (/ numerator denominator)
                            new-vi (utils/v+ (map #(* alpha %) vi)
                                             (map #(* (- 1 alpha) %) (get v (dec i))))
                            new-v (assoc v i new-vi)]
                        (recur new-vi (dec i)))
                      v)))
              v-outer)))))))

#_(def test-spline
  (let [degree 3
        pts [[0 0] [5 5] [10 -5] [15 25] [20 -5] [25 5] [30 0]]
        knots [1 2 3 4 5 6 7 8 9 10 11]]
    (partial b-spline-inner [pts degree knots] [1 pts])))
