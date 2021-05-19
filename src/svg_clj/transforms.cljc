(ns svg-clj.transforms
   (:require [clojure.string :as str]
             [svg-clj.utils :as utils]
             [svg-clj.path :as path]
            #?(:cljs
               [cljs.reader :refer [read-string]])))

(defmulti command->pts :command)

(defmethod command->pts :default
  [{:keys [input]}]
  (mapv vec (partition 2 input)))

;; this is not implemented correctly yet.
;; just a 'stub' returning the end point of the arc
(defmethod command->pts "A"
  [{:keys [input cursor]}]
  (let [[rx ry deg laf sw x y] input
        b [x y]
        #_ctr #_[(- x (* (Math/cos (utils/to-rad deg)) rx))
             (- y (* (Math/sin (utils/to-rad deg)) rx))]
        ctr (utils/v+ cursor [rx 0])
        sa (utils/angle-from-pts cursor ctr b)
        angle (if (= 1 laf) (- 360 sa) sa)
        mids (mapv #(utils/rotate-pt-around-center cursor % ctr) (rest (range 0 angle 90)))]
    (conj mids b)))

(defn style
  [[k props & content] style-map]
  (into [k (merge props style-map)] content))

(defn centroid-of-pts
  "Calculates the arithmetic mean position of the given `pts`."
  [pts]
  (let [ndim (count (first (sort-by count pts)))
        splits (for [axis (range 0 ndim)]
                 (map #(nth % axis) pts))]
    (mapv #(apply utils/average %) splits)))

(defmulti centroid
  (fn [element]
    (if (keyword? (first element))
      (first element)
      :list)))

(defmethod centroid :list
  [elems]
  (centroid-of-pts (into #{} (map centroid elems))))

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
    (centroid-of-pts [a b])))

(defmethod centroid :polygon
  [[_ props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))]
    (centroid-of-pts pts)))

(defmethod centroid :polyline
  [[_ props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))]
    (centroid-of-pts pts)))

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
  [[_ props text]]
  [(:x props) (:y props)])

(defmethod centroid :path
  [[_ props]]
  (let [cmds (path/path-string->commands (:d props))
        pts (mapcat command->pts cmds)]
    (centroid-of-pts (vec (into #{} pts)))))

(declare centroid)
(defmethod centroid :g
  [[_ props & content]]
  (centroid-of-pts (into #{} (map centroid content))))

(defn bounds-of-pts
  [pts]
  (let [xmax (apply max (map first pts))
        ymax (apply max (map second pts))
        xmin (apply min (map first pts))
        ymin (apply min (map second pts))]
    (vector [xmin ymin]
            [xmax ymin]
            [xmax ymax]
            [xmin ymax])))

(defmulti bounds
  (fn [element]
    (if (keyword? (first element))
      (first element)
      :list)))

(defmethod bounds :list
  [elems]
  (bounds-of-pts (mapcat bounds elems)))

(defmethod bounds :circle
  [[_ props]]
  (let [c [(:cx props) (:cy props)]
        r (:r props)
        pts (mapv #(utils/v+ c %) [[r 0]
                             [0 r]
                             [(- r) 0]
                             [0 (- r)]])]
    (bounds-of-pts pts)))

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
        bb (bounds-of-pts pts)
        obb (mapv #(utils/rotate-pt-around-center % deg [mx my]) bb)
        xpts (mapv #(utils/rotate-pt-around-center % deg [mx my]) pts)
        small-bb (bounds-of-pts xpts)
        large-bb (bounds-of-pts obb)]
    ;; not accurate, but good enough for now
    ;; take the bb to be the average between the small and large
    (bounds-of-pts (mapv #(centroid-of-pts [%1 %2]) small-bb large-bb))))

(defmethod bounds :line
  [[_ props]]
  (let [a (mapv #(get props %) [:x1 :y1])
        b (mapv #(get props %) [:x2 :y2])]
    (bounds-of-pts [a b])))

(defmethod bounds :polygon
  [[_ props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))]
    (bounds-of-pts pts)))

(defmethod bounds :polyline
  [[_ props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))]
    (bounds-of-pts pts)))

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
    (bounds-of-pts xpts)))

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
    (bounds-of-pts xpts)))

;; this is not done yet. Text in general needs a redo.
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
    (bounds-of-pts xpts)))

(defmethod bounds :path
  [[_ props]]
  (let [cmds (path/path-string->commands (:d props))
        pts (mapcat command->pts cmds)]
    (bounds-of-pts pts)))

(declare bounds)
(defmethod bounds :g
  [[_ props & content]]
  (bounds-of-pts (mapcat bounds content)))

(defn bb-dims
   "The svg fn wraps `content` in an SVG container element.
   The SVG container is parameterized by width `w`, height `h`, and scale `sc`."
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

(defmulti translate-path-command
  (fn [cmd _]
    (:command cmd)))

(defmethod translate-path-command "M"
  [{:keys [:input] :as m} [x y]]
  (assoc m :input (utils/v+ [x y] input)))

(defmethod translate-path-command "L"
  [{:keys [:input] :as m} [x y]]
  (assoc m :input (utils/v+ [x y] input)))

(defmethod translate-path-command "H"
  [{:keys [:input] :as m} [x y]]
  (assoc m :input (utils/v+ [x] input)))

(defmethod translate-path-command "V"
  [{:keys [:input] :as m} [x y]]
  (assoc m :input (utils/v+ [y] input)))

;; x y x y x y because input will ahve the form:
;; [x1 y1 x2 y2 x y] (first two pairs are control points)
(defmethod translate-path-command "C"
  [{:keys [:input] :as m} [x y]]
  (assoc m :input (utils/v+ [x y x y x y] input)))

;; similar approach to above, but one control point is implicit
(defmethod translate-path-command "S"
  [{:keys [:input] :as m} [x y]]
  (assoc m :input (utils/v+ [x y x y] input)))

(defmethod translate-path-command "Q"
  [{:keys [:input] :as m} [x y]]
  (assoc m :input (utils/v+ [x y x y] input)))

(defmethod translate-path-command "T"
  [{:keys [:input] :as m} [x y]]
  (assoc m :input (utils/v+ [x y] input)))

;; [rx ry xrot laf swf x y]
;; rx, ry do not change
;; xrot also no change
;; large arc flag and swf again no change
(defmethod translate-path-command "A"
  [{:keys [:input] :as m} [x y]]
  (let [[rx ry xrot laf swf ox oy] input]
    (assoc m :input [rx ry xrot laf swf (+ x ox) (+ y oy)])))

(defmethod translate-path-command "Z"
  [cmd _]
  cmd)

(defmethod translate-path-command :default
  [cmd a]
  [cmd a])

(defmethod translate :path
  [[k props] [x y]]
  (let [cmds (path/path-string->commands (:d props))
        xcmds (map #(translate-path-command % [x y]) cmds)]
    [k (assoc props :d (path/cmds->path-string xcmds))]))

#_(declare translate)
(defmethod translate :g
  [[k props & content] [x y]]
  (->> content
       (map #(translate % [x y]))
       (filter (complement nil?))
       (into [k props])))

(defn rotate-element-by-transform
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
                                (map #(utils/v- % (centroid-of-pts pts)))
                                (map #(utils/rotate-pt % deg))
                                (map #(utils/v+ % (centroid-of-pts pts))))
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

(defmulti rotate-path-command
  (fn [cmd _ _]
    (:command cmd)))

(defmethod rotate-path-command "M"
  [{:keys [:input] :as m} ctr deg]
  (let [xpt (-> input
                (utils/v- ctr)
                (utils/rotate-pt deg)
                (utils/v+ ctr))]
    (assoc m :input xpt)))

(defmethod rotate-path-command "L"
  [{:keys [:input] :as m} ctr deg]
  (let [xpt (-> input
                (utils/v- ctr)
                (utils/rotate-pt deg)
                (utils/v+ ctr))]
    (assoc m :input xpt)))

(defmethod rotate-path-command "C"
  [{:keys [:input] :as m} ctr deg]
  (let [xinput (->> input
                    (partition 2)
                    (map vec)
                    (map #(utils/v- % ctr))
                    (map #(utils/rotate-pt % deg))
                    (map #(utils/v+ % ctr))
                    (apply concat))]
    (assoc m :input xinput)))

(defmethod rotate-path-command "S"
  [{:keys [:input] :as m} ctr deg]
  (let [xinput (->> input
                    (partition 2)
                    (map vec)
                    (map #(utils/v- % ctr))
                    (map #(utils/rotate-pt % deg))
                    (map #(utils/v+ % ctr))
                    (apply concat))]
    (assoc m :input xinput)))

(defmethod rotate-path-command "Q"
  [{:keys [:input] :as m} ctr deg]
  (let [xinput (->> input
                    (partition 2)
                    (map vec)
                    (map #(utils/v- % ctr))
                    (map #(utils/rotate-pt % deg))
                    (map #(utils/v+ % ctr))
                    (apply concat))]
    (assoc m :input xinput)))

(defmethod rotate-path-command "T"
  [{:keys [:input] :as m} ctr deg]
  (let [xpt (-> input
                (utils/v- ctr)
                (utils/rotate-pt deg)
                (utils/v+ ctr))]
    (assoc m :input xpt)))

;; [rx ry xrot laf swf x y]
;; rx, ry do not change
;; xrot also no change
;; large arc flag and swf again no change
(defmethod rotate-path-command "A"
  [{:keys [:input] :as m} ctr deg]
  (let [[rx ry xrot laf swf ox oy] input
        [nx ny] (-> [ox oy]
                    (utils/v- ctr)
                    (utils/rotate-pt deg)
                    (utils/v+ ctr))]
    (assoc m :input [rx ry (+ xrot deg) laf swf nx ny])))

(defmethod rotate-path-command "Z"
  [cmd _ _]
  cmd)

(defmethod rotate :path
  [[k props] deg]
  (let [ctr (centroid [k props])
        cmds (path/path-string->commands (:d props))
        xcmds (map #(rotate-path-command % ctr deg) cmds)]
    [k (assoc props :d (path/cmds->path-string xcmds))]))

(defmethod rotate :g
  [[k props & content] deg]
  (let [[gcx gcy] (centroid-of-pts (bounds (into [k props] content)))
        xfcontent (for [child content]
                    (let [ch (translate child [(- gcx) (- gcy)])
                          ctr (if (= :g (first ch))
                                (centroid-of-pts (bounds ch))
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
  [[k props] [sx sy]]
  (let [circle? (= sx sy)
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
  (let [new-props (-> props
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

(defn scale-pt-from-center
  [[cx cy] [sx sy] [x y]]
  [(+ (* (- x cx) sx) cx)
   (+ (* (- y cy) sy) cy)])

(defmethod scale :polygon
  [[k props] [sx sy]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))
        ctr (centroid [k props])
        xpts (->> pts
                  (map (partial scale-pt-from-center ctr [sx sy]))
                  (map utils/v->s))]
    [k (assoc props :points (str/join " " xpts))]))

(defmethod scale :polyline
  [[k props] [sx sy]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))
        ctr (centroid [k props])
        xpts (->> pts
                  (map (partial scale-pt-from-center ctr [sx sy]))
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

(defmethod scale :g
  [[k props & content] [sx sy]]
  (let [xf (utils/str->xf-map (:transform props))
        new-xf (-> xf
                   (update :scale (fnil #(map * [sx sy] %) [1 1])))
        new-props (assoc props :transform (utils/xf-map->str new-xf))]
    (into [k new-props] content)))

(defmulti scale-path-command
  (fn [cmd _ _]
    (:command cmd)))

(defmethod scale-path-command :default
  [{:keys [:input] :as m} ctr [sx sy]]
  (let [pts (mapv vec (partition 2 input))
        xpts (->> pts
                  (mapcat (partial scale-pt-from-center ctr [sx sy])))]
    (assoc m :input (vec xpts))))

;; this is wrong. just a stub to get moving a bit
(defmethod scale-path-command "A"
  [{:keys [:input] :as m} ctr [sx sy]]
  (let [pts [(take-last 2 input)]
        xpts (->> pts
                  (mapcat (partial scale-pt-from-center ctr [sx sy])))]
    (assoc m :input (vec xpts))))

(defmethod scale :path
  [[k props] [sx sy]]
  (let [ctr (centroid [k props])
        cmds (path/path-string->commands (:d props))
        xcmds (map #(scale-path-command ctr % [sx sy]) cmds)]
    [k (assoc props :d (path/cmds->path-string xcmds))]))
