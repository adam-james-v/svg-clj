(ns svg-clj.transforms
  "Provides functions for computing and transforming properties of the SVG elements created by the `elements`, `path`, and `composites` namespaces.

  The most common transformations include translate, rotate, style, and scale which all work on every element. Other transformations include merge, split, and explode and these only work on path elements.

  This namespace also provides `bounds`, and `centroid` functions which calculate the respective property for all elements provided by this library."
  (:require [clojure.string :as str]   
            [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.path :as path]
            [svg-clj.parametric :as p]
            #?(:cljs
               [cljs.reader :refer [read-string]])))

(defn style
  [elem style-map]
  (utils/style elem style-map))

(defmulti centroid
  (fn [element]
    (if (keyword? (first element))
      (first element)
      :list)))

(defmethod centroid :list
  [elems]
  (utils/centroid-of-pts (into #{} (map centroid elems))))

(defmethod centroid :circle
  [[_ props]]
  [(:cx props) (:cy props)])  

(defmethod centroid :ellipse
  [[_ props]]
  [(:cx props) (:cy props)])

(defmethod centroid :line
  [[_ props]]
  (let [a (mapv #(get props %) [:x1 :y1])
        b (mapv #(get props %) [:x2 :y2])]
    (utils/centroid-of-pts [a b])))

(defmethod centroid :polygon
  [[_ props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))]
    (utils/centroid-of-pts pts)))

(defmethod centroid :polyline
  [[_ props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))]
    (utils/centroid-of-pts pts)))

(defmethod centroid :rect
  [[_ props]]
  [(+ (:x props) (/ (:width  props) 2.0))
   (+ (:y props) (/ (:height props) 2.0))])

(defmethod centroid :image
  [[_ props]]
  [(+ (:x props) (/ (:width  props) 2.0))
   (+ (:y props) (/ (:height props) 2.0))])

;; this is not done yet. Text in general needs a redo.
(defmethod centroid :text
  [[_ props _]]
  [(:x props) (:y props)])

(defmethod centroid :path
  [elem]
  (path/centroid elem))

(declare centroid)
(defmethod centroid :g
  [[_ _ & content]]
  (utils/centroid-of-pts (into #{} (map centroid content))))

(defmulti bounds
  "Calculates the axis-aligned-bounding-box of `element` or list of elements."
  (fn [element]
    (if (keyword? (first element))
      (first element)
      :list)))

(defmethod bounds :default
  [_]
  [[-1 -1] [1 -1] [1 1] [-1 1]])

(defmethod bounds :list
  [elems]
  (utils/bounds-of-pts (mapcat bounds elems)))

(defmethod bounds :circle
  [[_ props]]
  (let [c [(:cx props) (:cy props)]
        r (:r props)
        pts (mapv #(utils/v+ c %) [[r 0]
                             [0 r]
                             [(- r) 0]
                             [0 (- r)]])]
    (utils/bounds-of-pts pts)))

(defmethod bounds :ellipse
  [[_ props]]
  (let [xf (utils/str->xf-map  (get props :transform "rotate(0 0 0)"))
        deg (get-in xf [:rotate 0])
        mx (get-in xf [:rotate 1])
        my (get-in xf [:rotate 2])
        c [(:cx props) (:cy props)]
        rx (:rx props)
        ry (:ry props)
        pts (mapv #(utils/v+ c %) [[rx 0]
                                   [0 ry] 
                                   [(- rx) 0]
                                   [0 (- ry)]])
        bb (utils/bounds-of-pts pts)
        obb (mapv #(utils/rotate-pt-around-center % deg [mx my]) bb)
        xpts (mapv #(utils/rotate-pt-around-center % deg [mx my]) pts)
        small-bb (utils/bounds-of-pts xpts)
        large-bb (utils/bounds-of-pts obb)]
    ;; not accurate, but good enough for now
    ;; take the bb to be the average between the small and large
    (utils/bounds-of-pts (mapv #(utils/centroid-of-pts [%1 %2]) small-bb large-bb))))

(defmethod bounds :line
  [[_ props]]
  (let [a (mapv #(get props %) [:x1 :y1])
        b (mapv #(get props %) [:x2 :y2])]
    (utils/bounds-of-pts [a b])))

(defmethod bounds :polygon
  [[_ props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))]
    (utils/bounds-of-pts pts)))

(defmethod bounds :polyline
  [[_ props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))]
    (utils/bounds-of-pts pts)))

(defmethod bounds :rect
  [[_ props]]
  (let [xf (utils/str->xf-map (get props :transform "rotate(0 0 0)"))
        deg (get-in xf [:rotate 0])
        mx (get-in xf [:rotate 1])
        my (get-in xf [:rotate 2])
        x (:x props)
        y (:y props)
        w (:width props)
        h (:height props)
        pts [[x y]
             [(+ x w) y]
             [(+ x w) (+ y h)]
             [x (+ y h)]]
        xpts (mapv #(utils/rotate-pt-around-center % deg [mx my]) pts)]
    (utils/bounds-of-pts xpts)))

(defmethod bounds :image
  [[_ props]]
  (let [xf (utils/str->xf-map (get props :transform "rotate(0 0 0)"))
        deg (get-in xf [:rotate 0])
        mx (get-in xf [:rotate 1])
        my (get-in xf [:rotate 2])
        x (:x props)
        y (:y props)
        w (:width props)
        h (:height props)
        pts [[x y]
             [(+ x w) y]
             [(+ x w) (+ y h)]
             [x (+ y h)]]
        xpts (mapv #(utils/rotate-pt-around-center % deg [mx my]) pts)]
    (utils/bounds-of-pts xpts)))

;; this is not done yet. Text in general needs a redo.
;; Austin is a headless browser that may help with .getBBox???
(defmethod bounds :text
  [[_ {:keys [x y font-size ] :as props} text]]
  (let [xf (utils/str->xf-map (get props :transform "rotate(0 0 0)"))
        deg (get-in xf [:rotate 0])
        ar 0.6
        h (read-string (str font-size))
        hh (/ h 2.0)
        hw (/ (* ar h (count text)) 2.0)
        pts [ [(- x hw) (- y hh)]
             [(+ x hw) (- y hh)]
             [(+ x hw) (+ y hh)]
             [(- x hw) (+ y hh)] ]
        xpts (mapv #(utils/rotate-pt-around-center % deg [x y]) pts)]
    (utils/bounds-of-pts xpts)))

(defmethod bounds :path
  [elem]
  (path/bounds elem))

(declare bounds)
(defmethod bounds :g
  [[_ _ & content]]
  (utils/bounds-of-pts (mapcat bounds content)))

(defn bb-dims
  [element]
  (let [[[xmin ymin] _ [xmax ymax] _] (bounds element)]
    [(- xmax xmin) (- ymax ymin)]))

(defn- get-props
  [props]
  (merge {:rotate [0 0 0]} (utils/str->xf-map (get props :transform))))

(defmulti translate
  (fn [element _]
    (if (keyword? (first element))
      (first element)
      :list)))

(defmethod translate :list
  [elems [x y]]
  (map #(translate % [x y]) elems))

(defmethod translate :circle
  [[k props] [x y]]
  (let [xf (get-props props)
        cx (:cx props)
        cy (:cy props)
        new-xf (-> xf
                   (assoc-in [:rotate 1] (+ x cx))
                   (assoc-in [:rotate 2] (+ y cy)))
        new-props (-> props
                      (assoc :transform (utils/xf-map->str new-xf))
                      (update :cx + x)
                      (update :cy + y))]
    [k new-props]))

(defmethod translate :ellipse
  [[k props] [x y]]
  (let [xf (get-props props)
        cx (:cx props)
        cy (:cy props)
        new-xf (-> xf
                   (assoc-in [:rotate 1] (+ x cx))
                   (assoc-in [:rotate 2] (+ y cy)))
        new-props (-> props
                      (assoc :transform (utils/xf-map->str new-xf))
                      (update :cx + x)
                      (update :cy + y))]
    [k new-props]))

(defmethod translate :line
  [[k props] [x y]]
  (let [new-props (-> props
                      (update :x1 + x)
                      (update :y1 + y)
                      (update :x2 + x)
                      (update :y2 + y))]
    [k new-props]))

(defmethod translate :polygon
  [[k props] [x y]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))
        xpts (->> pts
                  (map (partial utils/v+ [x y]))
                  (map utils/v->s))]
    [k (assoc props :points (str/join " " xpts))]))

(defmethod translate :polyline
  [[k props] [x y]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))
        xpts (->> pts 
                  (map (partial utils/v+ [x y]))
                  (map utils/v->s))]
    [k (assoc props :points (str/join " " xpts))]))

(defmethod translate :rect
  [[k props] [x y]]
  (let [[cx cy] (centroid [k props])
        xf (get-props props)
        new-xf (-> xf
                   (assoc-in [:rotate 1] (+ cx x))
                   (assoc-in [:rotate 2] (+ cy y)))
        new-props (-> props
                      (assoc :transform (utils/xf-map->str new-xf))
                      (update :x + x)
                      (update :y + y))]
    [k new-props]))

(defmethod translate :image
  [[k props] [x y]]
  (let [[cx cy] (centroid [k props])
        xf (get-props props)
        new-xf (-> xf
                   (assoc-in [:rotate 1] (+ cx x))
                   (assoc-in [:rotate 2] (+ cy y)))
        new-props (-> props
                      (assoc :transform (utils/xf-map->str new-xf))
                      (update :x + x)
                      (update :y + y))]
    [k new-props]))

(defmethod translate :text
  [[k props text] [x y]]
  (let [xf (get-props props)
        new-xf (-> xf
                   (update-in [:rotate 1] + x)
                   (update-in [:rotate 2] + y))
        new-props (-> props
                      (assoc :transform (utils/xf-map->str new-xf))
                      (update :x + x)
                      (update :y + y))]
    [k new-props text]))

(defmethod translate :path
  [elem [x y]]
  (path/translate elem [x y]))

#_(declare translate)
(defmethod translate :g
  [[k props & content] [x y]]
  (->> content
       (map #(translate % [x y]))
       (filter (complement nil?))
       (into [k props])))

(defn rotate-element-by-transform
  "Rotate an element by using the SVG transform property.
  This function is used to transform elements that cannot 'bake' the transform into their other geometric properties. For example, the ellipse and circle elements have only center and radius properties which cannot affect orientation."
  [[k props content] deg]
  (let [xf (get-props props)
        new-xf (-> xf
                   (update-in [:rotate 0] + deg))
        new-props (assoc props :transform (utils/xf-map->str new-xf))]
    (vec (filter (complement nil?) [k new-props (when content content)]))))

(defmulti rotate
  (fn [element _]
    (if (keyword? (first element))
      (first element)
      :list)))

(defmethod rotate :list
  [elems deg]
  (map #(rotate % deg) elems))

(defmethod rotate :circle
  [[k props] deg]
  (rotate-element-by-transform [k props] deg))

(defmethod rotate :ellipse
  [[k props] deg]
  (rotate-element-by-transform [k props] deg))

(defmethod rotate :line
  [[k props] deg]
  (let [pts [[(:x1 props) (:y1 props)] [(:x2 props) (:y2 props)]]
        [[x1 y1] [x2 y2]]  (->> pts
                                (map #(utils/v- % (utils/centroid-of-pts pts)))
                                (map #(utils/rotate-pt % deg))
                                (map #(utils/v+ % (utils/centroid-of-pts pts))))
        new-props (assoc props :x1 x1 :y1 y1 :x2 x2 :y2 y2)]
    [k new-props]))

(defmethod rotate :polygon
  [[k props] deg]
  (let [ctr (centroid [k props])
        pts (mapv utils/s->v (str/split (:points props) #" "))
        xpts (->> pts
                  (map #(utils/v- % ctr))
                  (map #(utils/rotate-pt % deg))
                  (map #(utils/v+ % ctr))
                  (map utils/v->s))
        xprops (assoc props :points (str/join " " xpts))]
    [k xprops]))

(defmethod rotate :polyline
  [[k props] deg]
  (let [ctr (centroid [k props])
        pts (mapv utils/s->v (str/split (:points props) #" "))
        xpts (->> pts
                  (map #(utils/v- % ctr))
                  (map #(utils/rotate-pt % deg))
                  (map #(utils/v+ % ctr))
                  (map utils/v->s))
        xprops (assoc props :points (str/join " " xpts))]
    [k xprops]))

(defmethod rotate :rect
  [[k props] deg]
  (let [[cx cy] (centroid [k props])
        xf (get-props props)
        new-xf (-> xf
                   (update-in [:rotate 0] + deg)
                   (assoc-in  [:rotate 1] cx)
                   (assoc-in  [:rotate 2] cy))
        new-props (assoc props :transform (utils/xf-map->str new-xf))]
    [k new-props]))

(defmethod rotate :image
  [[k props] deg]
  (let [[cx cy] (centroid [k props])
        xf (get-props props)
        new-xf (-> xf
                   (update-in [:rotate 0] + deg)
                   (assoc-in  [:rotate 1] cx)
                   (assoc-in  [:rotate 2] cy))
        new-props (assoc props :transform (utils/xf-map->str new-xf))]
    [k new-props]))

(defmethod rotate :text
  [[k props text] deg]
  (rotate-element-by-transform [k props text] deg))

(defmethod rotate :path
  [elem deg]
  (path/rotate elem deg))

(defmethod rotate :g
  [[k props & content :as elem] deg]
  (let [[gcx gcy] (utils/centroid-of-pts (bounds elem))
        xfcontent (for [child content]
                    (let [ch (translate child [(- gcx) (- gcy)])
                          ctr (if (= :g (first ch))
                                (utils/centroid-of-pts (bounds ch))
                                (centroid ch))
                          xfm (-> ctr
                                  (utils/rotate-pt deg)
                                  (utils/v+ [gcx gcy]))]
                      (-> ch
                          (translate (utils/v* [-1 -1] ctr))
                          (rotate deg)
                          (translate xfm))))]
    (into [k props] (filter (complement nil?) xfcontent))))

(defn scale-by-transform
  [[k props & content] [sx sy]]
  (let [xf (utils/str->xf-map (:transform props))
        new-xf (-> xf
                   (update :scale (fnil #(map * [sx sy] %) [1 1])))
        new-props (assoc props :transform (utils/xf-map->str new-xf))]
    [k new-props] content))

(defmulti scale
  (fn [element _]
    (if (keyword? (first element))
      (first element)
      :list)))

(defmethod scale :list
  [elems [sx sy]]
  (map #(scale [sx sy] %) elems))

;; transforms are applied directly to the properties of shapes.
;; I have scale working the same way. One issue is that scaling a circle
;; turns it into an ellipse. This impl WILL change the shape to ellipse if non-uniform scaling is applied.

(defmethod scale :circle
  [[_ props] [sx sy]]
  (let [[sx sy] (map #(Math/abs %) [sx sy])
        circle? (= sx sy)
        r (:r props)
        new-props (if circle?
                    (assoc props :r (* r sx))
                    (-> props
                        (dissoc :r)
                        (assoc :rx (* sx r))
                        (assoc :ry (* sy r))))
        k (if circle? :circle :ellipse)]
    [k new-props]))

(defmethod scale :ellipse
  [[k props] [sx sy]]
  (let [[sx sy] (map #(Math/abs %) [sx sy])
        new-props (-> props
                      (update :rx #(* sx %))
                      (update :ry #(* sy %)))]
    [k new-props]))

;; find bounding box center
;; translate bb-center to 0 0
;; scale all x y values by * [sx sy]
;; translate back to original bb-center

(defmethod scale :line
  [[k props] [sx sy]]
  (let [[cx cy] (centroid [k props])
        new-props (-> props
                      (update :x1 #(+ (* (- % cx) sx) cx))
                      (update :y1 #(+ (* (- % cy) sy) cy))
                      (update :x2 #(+ (* (- % cx) sx) cx))
                      (update :y2 #(+ (* (- % cy) sy) cy)))]
    [k new-props]))

(defmethod scale :polygon
  [[k props] [sx sy]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))
        ctr (centroid [k props])
        xpts (->> pts
                  (map (partial utils/scale-pt-from-center ctr [sx sy]))
                  (map utils/v->s))]
    [k (assoc props :points (str/join " " xpts))]))

(defmethod scale :polyline
  [[k props] [sx sy]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))
        ctr (centroid [k props])
        xpts (->> pts
                  (map (partial utils/scale-pt-from-center ctr [sx sy]))
                  (map utils/v->s))]
    [k (assoc props :points (str/join " " xpts))]))

(defmethod scale :rect
  [[k props] [sx sy]]
  (let [cx (+ (:x props) (/ (:width props) 2.0))
        cy (+ (:y props) (/ (:height props) 2.0))
        w (* sx (:width props))
        h (* sy (:height props))
        new-props (-> props
                      (assoc :width w)
                      (assoc :height h)
                      (update :x #(+ (* (- % cx) sx) cx))
                      (update :y #(+ (* (- % cy) sy) cy)))]
    [k new-props]))

(defmethod scale :image
  [[k props] [sx sy]]
  (let [cx (+ (:x props) (/ (:width props) 2.0))
        cy (+ (:y props) (/ (:height props) 2.0))
        w (* sx (:width props))
        h (* sy (:height props))
        new-props (-> props
                      (assoc :width w)
                      (assoc :height h)
                      (update :x #(+ (* (- % cx) sx) cx))
                      (update :y #(+ (* (- % cy) sy) cy)))]
    [k new-props]))

(defmethod scale :text
  [[k props text] [sx sy]]
  (let [xf (get-props props)
        cx (get-in xf [:rotate 1])
        cy (get-in xf [:rotate 2])
        x (+ (* (- (:x props) cx) sx) cx)
        y (+ (* (- (:y props) cy) sy) cy)
        new-xf (-> xf
                   (assoc-in [:rotate 1] (- x))
                   (assoc-in [:rotate 2] (- y)))
        new-props (-> props
                      (assoc :transform (utils/xf-map->str new-xf))
                      (assoc :x x)
                      (assoc :y y)
                      (update-in [:style :font-size] #(* % sx)))]
    [k new-props text]))

(defmethod scale :path
  [elem [sx sy]]
  (path/scale elem [sx sy]))

#_(defmethod scale :g
  [[k props & content] [sx sy]]
  (let [xf (utils/str->xf-map (:transform props))
        new-xf (-> xf
                   (update :scale (fnil #(map * [sx sy] %) [1 1])))
        new-props (assoc props :transform (utils/xf-map->str new-xf))]
    (into [k new-props] content)))

(defmethod scale :g
  [[k props & content :as elem] [sx sy]]
  (let [g-ctr (utils/centroid-of-pts (bounds elem))
        xfcontent (for [child content]
                    (let [ch (scale child [sx sy])
                          elem-ctr (if (= :g (first ch))
                                     (utils/centroid-of-pts (bounds ch))
                                     (centroid ch))
                          elem-v (utils/v- elem-ctr g-ctr)]
                      (-> ch (translate (utils/v* [sx sy] elem-v)))))]
    (into [k props] (filter (complement nil?) xfcontent))))

(defn- offset-edge
  [[a b] d]
  (let [p (utils/perpendicular (utils/v- b a))
        pd (utils/v* (utils/normalize p) (repeat (- d)))
        xa (utils/v+ a pd)
        xb (utils/v+ b pd)]
    [xa xb]))

(defn- cycle-pairs
  [pts]
  (let [n (count pts)]
    (vec (take n (partition 2 1 (cycle pts))))))

(defn- wrap-list-once
  [s]
  (conj (drop-last s) (last s)))

(defn- every-other
  [v]
  (let [n (count v)]
    (map #(get v %) (filter even? (range n)))))

(defn offset-pts
  [pts d]
  (let [edges (cycle-pairs pts)
        opts (mapcat #(offset-edge % d) edges)
        oedges (every-other (cycle-pairs opts))
        edge-pairs (cycle-pairs oedges)]
    (wrap-list-once (map #(apply utils/line-intersection %) edge-pairs))))

(defmulti offset
  (fn [element _]
    (if (keyword? (first element))
      (first element)
      :list)))

(defmethod offset :default
  [[k _ :as elem]]
  (println (str "Offset not implemented for " k "."))
  elem)

(defmethod offset :list
  [elems d]
  (map #(offset % d) elems))

(defmethod offset :circle
  [[k props] d]
  (let [new-props (update props :r + d)]
    [k new-props]))

(defmethod offset :ellipse
  [[k props] d]
  (let [new-props (-> props
                      (update :rx + d)
                      (update :ry + d))]
    [k new-props]))

(defmethod offset :rect
  [[k props] d]
  (let [new-props (-> props
                      (update :x - d)
                      (update :y - d)
                      (update :width + (* d 2))
                      (update :height + (* d 2)))]
    [k new-props]))

(defmethod offset :line
  [[k {:keys [x1 y1 x2 y2] :as props}] d]
  (let [[[nx1 ny1] [nx2 ny2]] (offset-edge [[x1 y1] [x2 y2]] d)
        new-props (-> props
                      (assoc :x1 nx1)
                      (assoc :y1 ny1)
                      (assoc :x2 nx2)
                      (assoc :y2 ny2))]
    [k new-props]))

(defmethod offset :polygon
  [[k {:keys [points] :as props}] d]
  (let [pts (map vec (partition 2 (utils/s->v points)))
        opts (offset-pts pts d)
        npoints (str/join " " (map utils/v->s opts))
        new-props (assoc props :points npoints)]
    [k new-props]))

(defmethod offset :polyline
  [[k {:keys [points] :as props}] d]
  (let [pts (map vec (partition 2 (utils/s->v points)))
        opts (offset-pts pts d)
        npoints (str/join " " (map utils/v->s opts))
        new-props (assoc props :points npoints)]
    [k new-props]))
