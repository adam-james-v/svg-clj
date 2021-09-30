(ns svg-clj.utils
  (:require [clojure.string :as str]
            [clojure.data.xml :as xml]
            [clojure.walk :refer [postwalk]]
            [clojure.zip :as zip]
            [same :refer [zeroish?]]
            #?(:cljs
               [cljs.reader :refer [read-string]])))

(def ^:dynamic *rounding* nil)

(def abs #?(:clj #(Math/abs %)  :cljs js/Math.abs))
(def pow #?(:clj #(Math/pow %1 %2) :cljs js/Math.pow))

(defn round
  "Rounds a non-integer number `num` to `places` decimal places."
  ([num]
   (round num *rounding*))
  ([num places]
   (if places
     (let [d #?(:clj (bigdec (Math/pow 10 places))
                :cljs (Math/pow 10 places))]
       (double (/ (Math/round (* num d)) d)))
     num)))

;; vector arithmetic helpers
(def v+ (partial mapv +))
(def v- (partial mapv -))
(def v* (partial mapv *))

;; simple calcs
(defn to-deg
  [rad]
  (round (* rad (/ 180 Math/PI))))

(defn to-rad
  [deg]
  (round (* deg (/ Math/PI 180))))

(defn average
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
  "Turn transform maps from an element's properties into a string properly formatted for use inline in an svg element tag.

  Consider this an internal tool."
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
  "Computes the distance between two points `a` and `b`."
  [a b]
  (let [v (v- b a)
        v2 (reduce + (v* v v))]
    (round (Math/sqrt v2))))

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

(defn dot*
  "Calculates the dot product of two vectors."
  [a b]
  (reduce + (map * a b)))

(defn cross*
  "Calculates cross product of two 3d-vectors."
  [a b]
  (let [[a1 a2 a3] a
        [b1 b2 b3] b
        i (- (* a2 b3) (* a3 b2))
        j (- (* a3 b1) (* a1 b3))
        k (- (* a1 b2) (* a2 b1))]
    [i j k]))

(defn normalize
  "Calculates the unit vector of a given vector. Vector here is used in the mathematical sense."
  [v]
  (let [m (Math/sqrt (reduce + (v* v v)))]
    (mapv / v (repeat m))))

(defn normal
  "Calculates the normal vector of plane given 3 points or calculates the normal vector of a line given two (2D) points."
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
  "Using `p2` as the 'origin', return a string indicating positive, negative, or aligned to an axis for p1 p2."
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
        [v1nx v1ny] (normalize v1)
        [v2nx v2ny] (normalize v2)
        l1 (distance p1 p2)
        l2 (distance p3 p2)
        n (dot* v1 v2)
        d (* l1 l2)]
    (when-not (zeroish? (float d))
      (let [a (to-deg (Math/acos (/ n d)))
            quadrants (check-quadrants p1 p2 p3)]
        (cond
          ;; within same quadrant or one away
          (#{"p_p_" "p_pp" "p_np" "ppp_" "pppn"} quadrants) a 
          (#{"_p_p" "_pnp" "_pnn" "np_p" "npnn"} quadrants) a
          (#{"n_n_" "n_nn" "n_pn" "nnn_" "nnpn"} quadrants) a
          (#{"_n_n" "_npn" "_npp" "pn_n" "pnpp"} quadrants) a

          ;; same quadrant, checking if V2 is
          (and (= "pppp" quadrants) (> v2nx v1nx)) a
          (and (= "npnp" quadrants) (> v2nx v1nx)) a
          (and (= "nnnn" quadrants) (< v2nx v1nx)) a
          (and (= "pnpn" quadrants) (< v2nx v1nx)) a
          
          :else
          (+ (- 180 a) 180))))))

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
  [[pt-a pt-b] [pt-c pt-d]]
  (let [[ax ay] pt-a
        [bx by] pt-b
        [cx cy] pt-c
        [dx dy] pt-d
        xdiff [(- ax bx) (- cx dx)]
        ydiff [(- ay by) (- cy dy)]
        div (determinant xdiff ydiff)]
    (when (not (zeroish? (abs div)))
      (let [dets [(determinant pt-a pt-b) (determinant pt-c pt-d)]
            x (/ (determinant dets xdiff) div)
            y (/ (determinant dets ydiff) div)]
        [x y]))))

(defn style
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
  (let [xmax (apply max (map first pts))
        ymax (apply max (map second pts))
        xmin (apply min (map first pts))
        ymin (apply min (map second pts))]
    (vector [xmin ymin]
            [xmax ymin]
            [xmax ymax]
            [xmin ymax])))

(defn bb-dims
  [pts]
  (let [[[xmin ymin] _ [xmax ymax] _] (bounds-of-pts pts)]
    [(- xmax xmin) (- ymax ymin)]))

(defn str->number
  [s]
  (let [n (try (read-string s)
               (catch Exception e s))]
    (if (number? n) n s)))

(def numerical-attrs
  #{;; circle, ellipse
    :cx :cy :r :rx :ry
    ;; image, rect
    :width :height :x :y
    ;; line
    :x1 :y1 :x2 :y2})

(defn cast-numerical-attrs
  "Casts certain attribute values to numbers if they are strings.
Attributes to be cast are defined in `numerical-attrs` and include `:cx`, `:cy`, `:width`, etc."
  [attrs]
  (apply merge
         (map
          (fn [[k v]]
            (if (numerical-attrs k)
              {k (str->number v)}
              {k v}))
          attrs)))

(defn- fix-ns-tag
  [t]
  (let [namespace (namespace t)
        name (name t)]
    (if namespace
      (-> namespace
          (str/split #"\.")
          first
          (str ":" name)
          keyword)
      t)))

(defn xml->hiccup
  [xml]
  (if-let [t (:tag xml)]
    (let [elem [(fix-ns-tag t)]
          elem (if-let [attrs (:attrs xml)]
                (conj elem (cast-numerical-attrs attrs))
                elem)]
      (into elem (map xml->hiccup (remove string? (:content xml)))))
    xml))

(defn svg-str->hiccup
  "Parses an SVG string into a Hiccup data structure, keeping all nodes."
  [svg-str]
  (-> svg-str
      (xml/parse-str :namespace-aware false)
      xml->hiccup))

(defn- get-nodes
  "Returns a list of nodes from `zipper` that return `true` from the `matcher` predicate fn.
  The `matcher` fn expects a zipper location, `loc`, and returns `true` (or some value) or `false` (or nil)."
  [zipper matcher]
  (loop [loc zipper
         acc []]
    (if (zip/end? loc)
      acc
      (if (matcher loc)
        (recur (zip/next loc) (conj acc (zip/node loc)))
        (recur (zip/next loc) acc)))))

(defn- elem-node?
  [loc key-set]
  (let [node (zip/node loc)]
    (if (keyword? (first node))
      (not (nil? (key-set (first node)))))))

(defn- hiccup-zip
  [tree]
  (let [branch? #(and (seqable? %) (not (map? %)) (not (string? %)))
        children (fn [x]
                   (let [c (remove map? (rest x))]
                     (when-not (empty? c) c)))
        make-node (fn [_ c] (when-not (empty? c) (vec c)))]
    (zip/zipper branch? children make-node tree)))

(def svg-element-keys #{:circle :ellipse
                        :line :rect
                        :polygon :polyline
                        :image :text :g})

(defn get-elems
  "Get SVG elements from `tree`, a Hiccup data structure.
Optionally, pass in a set of keys  as `key-set` to use when matching nodes from the tree."
  ([tree] (get-elems tree svg-element-keys))
  ([tree key-set]
   (let [zipper (hiccup-zip tree)]
    (apply list (get-nodes zipper #(elem-node? % key-set))))))

(defn svg-str->elems
  "Parses an SVG string into a sequence of SVG elements compatible with this library.
Elements are "
  ([svg-str] (svg-str->elems svg-str svg-element-keys))
  ([svg-str key-set]
   (-> svg-str
       (xml/parse-str :namespace-aware false)
       xml->hiccup
       (get-elems key-set))))
