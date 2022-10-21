(ns svg-clj.transforms
  "Provides functions for computing and transforming properties of the SVG elements created by the `elements`, `path`, and `composites` namespaces.

  The most common transformations include translate, rotate, style, and scale which all work on every element. Other transformations include merge, split, and explode and these only work on path elements.

  This namespace also provides `bounds`, and `centroid` functions which calculate the respective property for all elements provided by this library."
  (:require [clojure.string :as str]
            [svg-clj.path :as path]
            [svg-clj.utils :as u]
            #?(:cljs
               [cljs.reader :refer [read-string]])))

(defn style
  "Merge a style map into the given element."
  [elem style-map]
  (u/style elem style-map))

(defmulti centroid
  "Calculates the arithmetic mean position of the given `element`."
  (fn [element]
    (if (keyword? (first element))
      (first element)
      :list)))

(defmethod centroid :list
  [elems]
  (u/centroid-of-pts (into #{} (map centroid elems))))

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
    (u/centroid-of-pts [a b])))

(defmethod centroid :polygon
  [[_ props]]
  (let [pts (mapv u/s->v (str/split (:points props) #" "))]
    (u/centroid-of-pts pts)))

(defmethod centroid :polyline
  [[_ props]]
  (let [pts (mapv u/s->v (str/split (:points props) #" "))]
    (u/centroid-of-pts pts)))

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
  (u/centroid-of-pts (into #{} (map centroid content))))

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
  (u/bounds-of-pts (mapcat bounds elems)))

(defmethod bounds :circle
  [[_ props]]
  (let [c [(:cx props) (:cy props)]
        r (:r props)
        pts (mapv #(u/v+ c %) [[r 0]
                             [0 r]
                             [(- r) 0]
                             [0 (- r)]])]
    (u/bounds-of-pts pts)))

(defmethod bounds :ellipse
  [[_ props]]
  (let [xf (u/str->xf-map  (get props :transform "rotate(0 0 0)"))
        deg (get-in xf [:rotate 0])
        mx (get-in xf [:rotate 1])
        my (get-in xf [:rotate 2])
        c [(:cx props) (:cy props)]
        rx (:rx props)
        ry (:ry props)
        pts (mapv #(u/v+ c %) [[rx 0]
                                   [0 ry]
                                   [(- rx) 0]
                                   [0 (- ry)]])
        bb (u/bounds-of-pts pts)
        obb (mapv #(u/rotate-pt-around-center % deg [mx my]) bb)
        xpts (mapv #(u/rotate-pt-around-center % deg [mx my]) pts)
        small-bb (u/bounds-of-pts xpts)
        large-bb (u/bounds-of-pts obb)]
    ;; not accurate, but good enough for now
    ;; take the bb to be the average between the small and large
    (u/bounds-of-pts (mapv #(u/centroid-of-pts [%1 %2]) small-bb large-bb))))

(defmethod bounds :line
  [[_ props]]
  (let [a (mapv #(get props %) [:x1 :y1])
        b (mapv #(get props %) [:x2 :y2])]
    (u/bounds-of-pts [a b])))

(defmethod bounds :polygon
  [[_ props]]
  (let [pts (mapv u/s->v (str/split (:points props) #" "))]
    (u/bounds-of-pts pts)))

(defmethod bounds :polyline
  [[_ props]]
  (let [pts (mapv u/s->v (str/split (:points props) #" "))]
    (u/bounds-of-pts pts)))

(defmethod bounds :rect
  [[_ props]]
  (let [xf (u/str->xf-map (get props :transform "rotate(0 0 0)"))
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
        xpts (mapv #(u/rotate-pt-around-center % deg [mx my]) pts)]
    (u/bounds-of-pts xpts)))

(defmethod bounds :image
  [[_ props]]
  (let [xf (u/str->xf-map (get props :transform "rotate(0 0 0)"))
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
        xpts (mapv #(u/rotate-pt-around-center % deg [mx my]) pts)]
    (u/bounds-of-pts xpts)))

;; this is not done yet. Text in general needs a redo.
;; Austin is a headless browser that may help with .getBBox???
(defmethod bounds :text
  [[_ {:keys [x y font-size ] :as props} text]]
  (let [xf (u/str->xf-map (get props :transform "rotate(0 0 0)"))
        deg (get-in xf [:rotate 0])
        ar 0.6
        h (read-string (str font-size))
        hh (/ h 2.0)
        hw (/ (* ar h (count text)) 2.0)
        pts [ [(- x hw) (- y hh)]
             [(+ x hw) (- y hh)]
             [(+ x hw) (+ y hh)]
             [(- x hw) (+ y hh)] ]
        xpts (mapv #(u/rotate-pt-around-center % deg [x y]) pts)]
    (u/bounds-of-pts xpts)))

(defmethod bounds :path
  [elem]
  (path/bounds elem))

(declare bounds)
(defmethod bounds :g
  [[_ _ & content]]
  (u/bounds-of-pts (mapcat bounds content)))

(defn- get-props
  [props]
  (merge {:rotate [0 0 0]} (u/str->xf-map (get props :transform))))

(defmulti translate
  "Translates `element` by [`x` `y`]."
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
                      (assoc :transform (u/xf-map->str new-xf))
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
                      (assoc :transform (u/xf-map->str new-xf))
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
  (let [pts (mapv u/s->v (str/split (:points props) #" "))
        xpts (->> pts
                  (map (partial u/v+ [x y]))
                  (map u/v->s))]
    [k (assoc props :points (str/join " " xpts))]))

(defmethod translate :polyline
  [[k props] [x y]]
  (let [pts (mapv u/s->v (str/split (:points props) #" "))
        xpts (->> pts
                  (map (partial u/v+ [x y]))
                  (map u/v->s))]
    [k (assoc props :points (str/join " " xpts))]))

(defmethod translate :rect
  [[k props] [x y]]
  (let [[cx cy] (centroid [k props])
        xf (get-props props)
        new-xf (-> xf
                   (assoc-in [:rotate 1] (+ cx x))
                   (assoc-in [:rotate 2] (+ cy y)))
        new-props (-> props
                      (assoc :transform (u/xf-map->str new-xf))
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
                      (assoc :transform (u/xf-map->str new-xf))
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
                      (assoc :transform (u/xf-map->str new-xf))
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
        new-props (assoc props :transform (u/xf-map->str new-xf))]
    (vec (filter (complement nil?) [k new-props (when content content)]))))

(defmulti rotate
  "Rotate `element` by `deg` degrees around its centroid."
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
                                (map #(u/v- % (u/centroid-of-pts pts)))
                                (map #(u/rotate-pt % deg))
                                (map #(u/v+ % (u/centroid-of-pts pts))))
        new-props (assoc props :x1 x1 :y1 y1 :x2 x2 :y2 y2)]
    [k new-props]))

(defmethod rotate :polygon
  [[k props] deg]
  (let [ctr (centroid [k props])
        pts (mapv u/s->v (str/split (:points props) #" "))
        xpts (->> pts
                  (map #(u/v- % ctr))
                  (map #(u/rotate-pt % deg))
                  (map #(u/v+ % ctr))
                  (map u/v->s))
        xprops (assoc props :points (str/join " " xpts))]
    [k xprops]))

(defmethod rotate :polyline
  [[k props] deg]
  (let [ctr (centroid [k props])
        pts (mapv u/s->v (str/split (:points props) #" "))
        xpts (->> pts
                  (map #(u/v- % ctr))
                  (map #(u/rotate-pt % deg))
                  (map #(u/v+ % ctr))
                  (map u/v->s))
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
        new-props (assoc props :transform (u/xf-map->str new-xf))]
    [k new-props]))

(defmethod rotate :image
  [[k props] deg]
  (let [[cx cy] (centroid [k props])
        xf (get-props props)
        new-xf (-> xf
                   (update-in [:rotate 0] + deg)
                   (assoc-in  [:rotate 1] cx)
                   (assoc-in  [:rotate 2] cy))
        new-props (assoc props :transform (u/xf-map->str new-xf))]
    [k new-props]))

(defmethod rotate :text
  [[k props text] deg]
  (rotate-element-by-transform [k props text] deg))

(defmethod rotate :path
  [elem deg]
  (path/rotate elem deg))

(defmethod rotate :g
  [[k props & content :as elem] deg]
  (let [[gcx gcy] (u/centroid-of-pts (bounds elem))
        xfcontent (for [child content]
                    (let [ch (translate child [(- gcx) (- gcy)])
                          ctr (if (= :g (first ch))
                                (u/centroid-of-pts (bounds ch))
                                (centroid ch))
                          xfm (-> ctr
                                  (u/rotate-pt deg)
                                  (u/v+ [gcx gcy]))]
                      (-> ch
                          (translate (u/v* [-1 -1] ctr))
                          (rotate deg)
                          (translate xfm))))]
    (into [k props] (filter (complement nil?) xfcontent))))

(defn scale-by-transform
  "Scale an element by using the SVG transform property.
  This function is used to transform elements that cannot 'bake' the transform into their other geometric properties."
  [[k props & content] [sx sy]]
  (let [xf (u/str->xf-map (:transform props))
        new-xf (-> xf
                   (update :scale (fnil #(map * [sx sy] %) [1 1])))
        new-props (assoc props :transform (u/xf-map->str new-xf))]
    [k new-props] content))

(defmulti scale
  "Scale `element` by [`sx` `sy`] around its centroid."
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
  [[k props :as elem] [sx sy]]
  (let [[cx cy] (centroid elem)
        new-props (-> props
                      (update :x1 #(+ (* (- % cx) sx) cx))
                      (update :y1 #(+ (* (- % cy) sy) cy))
                      (update :x2 #(+ (* (- % cx) sx) cx))
                      (update :y2 #(+ (* (- % cy) sy) cy)))]
    [k new-props]))

(defmethod scale :polygon
  [[k props :as elem] [sx sy]]
  (let [pts (map vec (partition 2 (u/s->v (:points props))))
        ctr (centroid elem)
        xpts (->> pts
                  (map #(u/scale-pt-from-center % [sx sy] ctr))
                  (map u/v->s))]
    [k (assoc props :points (str/join " " xpts))]))

(defmethod scale :polyline
  [[k props :as elem] [sx sy]]
  (let [pts (map vec (partition 2 (u/s->v (:points props))))
        ctr (centroid elem)
        xpts (->> pts
                  (map #(u/scale-pt-from-center % [sx sy] ctr))
                  (map u/v->s))]
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
                      (assoc :transform (u/xf-map->str new-xf))
                      (assoc :x x)
                      (assoc :y y)
                      (update-in [:style :font-size] #(* % sx)))]
    [k new-props text]))

(defmethod scale :path
  [elem [sx sy]]
  (path/scale elem [sx sy]))

#_(defmethod scale :g
  [[k props & content] [sx sy]]
  (let [xf (u/str->xf-map (:transform props))
        new-xf (-> xf
                   (update :scale (fnil #(map * [sx sy] %) [1 1])))
        new-props (assoc props :transform (u/xf-map->str new-xf))]
    (into [k new-props] content)))

(defmethod scale :g
  [[k props & content :as elem] [sx sy]]
  (let [g-ctr (u/centroid-of-pts (bounds elem))
        xfcontent (for [child content]
                    (let [elem-ctr (if (= :g (first child))
                                     (u/centroid-of-pts (bounds child))
                                     (centroid child))
                          ch (-> child
                                 (translate (u/v* [-1 -1] elem-ctr))
                                 (scale [sx sy]))
                          elem-v (u/v- elem-ctr g-ctr)]
                      (-> ch (translate (u/v+ (u/v* [sx sy] elem-v) g-ctr)))))]
    (into [k props] (filter (complement nil?) xfcontent))))

(defmulti offset
  "Offset the boundary of `element` by distance `d`.
  The offset direction is always normal to the boundary, pointing outward if the path is wound in a CCW direction around the element's centroid."
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
  (let [[[nx1 ny1] [nx2 ny2]] (u/offset-edge [[x1 y1] [x2 y2]] d)
        new-props (-> props
                      (assoc :x1 nx1)
                      (assoc :y1 ny1)
                      (assoc :x2 nx2)
                      (assoc :y2 ny2))]
    [k new-props]))

(defmethod offset :polygon
  [[k {:keys [points] :as props}] d]
  (let [pts (map vec (partition 2 (u/s->v points)))
        opts (u/offset-pts pts d)
        npoints (str/join " " (map u/v->s opts))
        new-props (assoc props :points npoints)]
    [k new-props]))

(defmethod offset :polyline
  [[k {:keys [points] :as props}] d]
  (let [pts (map vec (partition 2 (u/s->v points)))
        opts (u/offset-pts pts d)
        npoints (str/join " " (map u/v->s opts))
        new-props (assoc props :points npoints)]
    [k new-props]))

(defmulti ^:private offset-path-command
  "Offset the path command `cmd`."
  (fn [cmd _]
    (:command cmd)))

(defmethod offset-path-command "M"
  [{:keys [:input] :as m} [x y]]
  (assoc m :input (u/v+ [x y] input)))

(defmethod offset-path-command "L"
  [{:keys [:input] :as m} [x y]]
  (assoc m :input (u/v+ [x y] input)))

(defmethod offset-path-command "H"
  [{:keys [:input] :as m} [x _]]
  (assoc m :input (u/v+ [x] input)))

(defmethod offset-path-command "V"
  [{:keys [:input] :as m} [_ y]]
  (assoc m :input (u/v+ [y] input)))

;; x y x y x y because input will have the form:
;; [x1 y1 x2 y2 x y] (first two pairs are control points)
(defmethod offset-path-command "C"
  [{:keys [:input] :as m} [x y]]
  (assoc m :input (u/v+ [x y x y x y] input)))

;; similar approach to above, but one control point is implicit
(defmethod offset-path-command "S"
  [{:keys [:input] :as m} [x y]]
  (assoc m :input (u/v+ [x y x y] input)))

(defmethod offset-path-command "Q"
  [{:keys [:input] :as m} [x y]]
  (assoc m :input (u/v+ [x y x y] input)))

(defmethod offset-path-command "T"
  [{:keys [:input] :as m} [x y]]
  (assoc m :input (u/v+ [x y] input)))

;; [rx ry xrot laf swf x y]
;; rx, ry do not change
;; xrot also no change
;; large arc flag and swf again no change
(defmethod offset-path-command "A"
  [{:keys [:input] :as m} [x y]]
  (let [[rx ry xrot laf swf ox oy] input]
    (assoc m :input [rx ry xrot laf swf (+ x ox) (+ y oy)])))

(defmethod offset-path-command "Z"
  [cmd _]
  cmd)

(defmethod offset-path-command :default
  [cmd a]
  [cmd a])

;; todo: TDD this offset implementation
(defmethod offset :path
  [[k props] d]
  (let [cmds (path/path-str->cmds (:d props))
        xcmds (map #(offset-path-command % d) cmds)]
    [k (assoc props :d (path/cmds->path-string xcmds))]))
