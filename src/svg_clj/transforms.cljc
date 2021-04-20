(ns svg-clj.transforms
   (:require [clojure.string :as str]
             [clojure.spec.alpha :as s]
             [svg-clj.specs :as specs]
             [svg-clj.utils :as utils :refer [move-pt
                                              rotate-pt
                                              rotate-pt-around-center]]
             [svg-clj.path :as path]))

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
        mids (mapv #(rotate-pt-around-center % ctr cursor) (rest (range 0 angle 90)))]
    (conj mids b)))

(defn centroid-of-pts
  "Calculates the arithmetic mean position of all the given `pts`."
  [pts]
  (let [ndim (count (first (sort-by count pts)))
        splits (for [axis (range 0 ndim)]
                 (map #(nth % axis) pts))]
    (mapv #(apply utils/average %) splits)))

(defmulti centroid-element
  (fn [element]
    (first element)))

(defmethod centroid-element :circle
  [[_ props]]
  [(:cx props) (:cy props)])  

(defmethod centroid-element :ellipse
  [[_ props]]
  [(:cx props) (:cy props)])

(defmethod centroid-element :line
  [[_ props]]
  (let [a (mapv #(get props %) [:x1 :y1])
        b (mapv #(get props %) [:x2 :y2])]
    (centroid-of-pts [a b])))

(defmethod centroid-element :polygon
  [[_ props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))]
    (centroid-of-pts pts)))

(defmethod centroid-element :polyline
  [[_ props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))]
    (centroid-of-pts pts)))

(defmethod centroid-element :rect
  [[_ props]]
  [(+ (:x props) (/ (:width  props) 2.0))
   (+ (:y props) (/ (:height props) 2.0))])

(defmethod centroid-element :image
  [[_ props]]
  [(+ (:x props) (/ (:width  props) 2.0))
   (+ (:y props) (/ (:height props) 2.0))])

;; this is not done yet. Text in general needs a redo.
(defmethod centroid-element :text
  [[_ props text]]
  [(:x props) (:y props)])

(defmethod centroid-element :path
  [[_ props]]
  (let [cmds (path/path-string->commands (:d props))
        pts (mapcat command->pts cmds)]
    (centroid-of-pts (vec (into #{} pts)))))

(declare centroid)
(defmethod centroid-element :g
  [[_ props & content]]
  (centroid-of-pts (into #{} (map centroid content))))

(defn centroid
  "Calculates the arithmetic mean position of all points of all given `elems`."
  [& elems]
  (if (and (= 1 (count elems))
           (not (keyword? (first (first elems)))))
    ;; content is a list of a list of elements
    (recur (first elems))
    ;; content is a single element OR a list of elements
    (centroid-of-pts (mapv centroid-element elems))))

(defn pts->bounds
  [pts]
  (let [xmax (apply max (map first pts))
        ymax (apply max (map second pts))
        xmin (apply min (map first pts))
        ymin (apply min (map second pts))]
    (vector [xmin ymin]
            [xmax ymin]
            [xmax ymax]
            [xmin ymax])))

(defmulti bounds-element
  (fn [element]
    (first element)))

(defmethod bounds-element :circle
  [[_ props]]
  (let [c [(:cx props) (:cy props)]
        r (:r props)
        pts (mapv #(utils/v+ c %) [[r 0]
                             [0 r]
                             [(- r) 0]
                             [0 (- r)]])]
    (pts->bounds pts)))

(defmethod bounds-element :ellipse
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
        bb (pts->bounds pts)
        obb (mapv #(rotate-pt-around-center deg [mx my] %) bb)
        xpts (mapv #(rotate-pt-around-center deg [mx my] %) pts)
        small-bb (pts->bounds xpts)
        large-bb (pts->bounds obb)]
    ;; not accurate, but good enough for now
    ;; take the bb to be the average between the small and large
    (pts->bounds (mapv #(centroid-of-pts [%1 %2]) small-bb large-bb))))

(defmethod bounds-element :line
  [[_ props]]
  (let [a (mapv #(get props %) [:x1 :y1])
        b (mapv #(get props %) [:x2 :y2])]
    (pts->bounds [a b])))

(defmethod bounds-element :polygon
  [[_ props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))]
    (pts->bounds pts)))

(defmethod bounds-element :polyline
  [[_ props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))]
    (pts->bounds pts)))

(defmethod bounds-element :rect
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
        xpts (mapv #(rotate-pt-around-center deg [mx my] %) pts)]
    (pts->bounds xpts)))

(defmethod bounds-element :image
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
        xpts (mapv #(rotate-pt-around-center deg [mx my] %) pts)]
    (pts->bounds xpts)))

;; this is not done yet. Text in general needs a redo.
(defmethod bounds-element :text
  [[_ props text]]
  [[(:x props) (:y props)]])

(defmethod bounds-element :path
  [[_ props]]
  (let [cmds (path/path-string->commands (:d props))
        pts (mapcat command->pts cmds)]
    (pts->bounds pts)))

(declare bounds)
(defmethod bounds-element :g
  [[_ props & content]]
  (pts->bounds (apply concat (map bounds content))))

(defn bounds
  "Calculates the axis-aligned bounding box of `elems`.
  The returned bounding box is a list of four points:
  [Bottom Left, Bottom Right, Top Right, Top Left]."
  [& elems]
  (if (and (= 1 (count elems))
           (not (keyword? (first (first elems)))))
    ;; content is a list of a list of elements
    (recur (first elems))
    ;; content is a single element OR a list of elements
    (pts->bounds (mapcat bounds-element elems))))

(defmulti translate-element 
  (fn [_ element]
    (first element)))

(defmethod translate-element :circle
  [[x y] [k props]]
  (let [xf (utils/str->xf-map (get props :transform "rotate(0 0 0)"))
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

(defmethod translate-element :ellipse
  [[x y] [k props]]
  (let [xf (utils/str->xf-map (get props :transform "rotate(0 0 0)"))
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

(defmethod translate-element :line
  [[x y] [k props]]
  (let [new-props (-> props
                      (update :x1 + x)
                      (update :y1 + y)
                      (update :x2 + x)
                      (update :y2 + y))]
    [k new-props]))

(defmethod translate-element :polygon
  [[x y] [k props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))
        xpts (->> pts 
                  (map (partial utils/v+ [x y]))
                  (map utils/v->s))]
    [k (assoc props :points (apply str (interpose " " xpts)))]))

(defmethod translate-element :polyline
  [[x y] [k props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))
        xpts (->> pts 
                  (map (partial utils/v+ [x y]))
                  (map utils/v->s))]
    [k (assoc props :points (apply str (interpose " " xpts)))]))

(defmethod translate-element :rect
  [[x y] [k props]]
  (let [[cx cy] (centroid [k props])
        xf (utils/str->xf-map (get props :transform "rotate(0 0 0)"))
        new-xf (-> xf
                   (assoc-in [:rotate 1] (+ cx x))
                   (assoc-in [:rotate 2] (+ cy y)))
        new-props (-> props
                      (assoc :transform (utils/xf-map->str new-xf))
                      (update :x + x)
                      (update :y + y))]
    [k new-props]))

(defmethod translate-element :image
  [[x y] [k props]]
  (let [[cx cy] (centroid [k props])
        xf (utils/str->xf-map (get props :transform "rotate(0 0 0)"))
        new-xf (-> xf
                   (assoc-in [:rotate 1] (+ cx x))
                   (assoc-in [:rotate 2] (+ cy y)))
        new-props (-> props
                      (assoc :transform (utils/xf-map->str new-xf))
                      (update :x + x)
                      (update :y + y))]
    [k new-props]))

(defmethod translate-element :text
  [[x y] [k props text]]
  (let [xf (utils/str->xf-map (get props :transform "rotate(0 0 0)"))
        new-xf (-> xf
                   (update-in [:rotate 1] + x)
                   (update-in [:rotate 2] + y))
        new-props (-> props
                      (assoc :transform (utils/xf-map->str new-xf))
                      (update :x + x)
                      (update :y + y))]
    [k new-props text]))

(defmulti translate-path-command
  (fn [_ m]
    (:command m)))

(defmethod translate-path-command "M"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (utils/v+ [x y] input)))

(defmethod translate-path-command "L"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (utils/v+ [x y] input)))

(defmethod translate-path-command "H"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (utils/v+ [x] input)))

(defmethod translate-path-command "V"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (utils/v+ [y] input)))

;; x y x y x y because input will ahve the form:
;; [x1 y1 x2 y2 x y] (first two pairs are control points)
(defmethod translate-path-command "C"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (utils/v+ [x y x y x y] input)))

;; similar approach to above, but one control point is implicit
(defmethod translate-path-command "S"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (utils/v+ [x y x y] input)))

(defmethod translate-path-command "Q"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (utils/v+ [x y x y] input)))

(defmethod translate-path-command "T"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (utils/v+ [x y] input)))

;; [rx ry xrot laf swf x y]
;; rx, ry do not change
;; xrot also no change
;; large arc flag and swf again no change
(defmethod translate-path-command "A"
  [[x y] {:keys [:input] :as m}]
  (let [[rx ry xrot laf swf ox oy] input]
    (assoc m :input [rx ry xrot laf swf (+ x ox) (+ y oy)])))

(defmethod translate-path-command "Z"
  [_ cmd]
  cmd)

(defmethod translate-path-command :default
  [a cmd]
  [a cmd])

(defmethod translate-element :path
  [[x y] [k props]]
  (let [cmds (path/path-string->commands (:d props))
        xcmds (map #(translate-path-command [x y] %) cmds)]
    [k (assoc props :d (path/cmds->path-string xcmds))]))

(declare translate)
(defmethod translate-element :g
  [[x y] [k props & content]]
  (->> content
       (map (partial translate [x y]))
       (filter (complement nil?))
       (into [k props])))

(defn translate
  "Translates the `elems` by `x` and `y` relative to the element(s)'s current position(s).

  For example, a shape sitting at [10 10] being translated by [10 10] will be located at [20 20] after translation."
  [[x y] & elems]
  (let [elem (first elems)
        elems (rest elems)]
    (when elem
      (cond
        (and (specs/element? elem) (= 0 (count elems)))
        (translate-element [x y] elem)
        (and (specs/element? elem) (< 0 (count elems)))
        (concat
         [(translate-element [x y] elem)]
         [(translate [x y] elems)])
        :else
        (recur [x y] (concat elem elems))))))

(defn translate2
  [[x y] & elems]
  (map #(translate-element [x y] %) (first elems)))

(defn rotate-element-by-transform
  [deg [k props content]]
  (let [xf (utils/str->xf-map (get props :transform "rotate(0 0 0)"))
        new-xf (-> xf
                   (update-in [:rotate 0] + deg))
        new-props (assoc props :transform (utils/xf-map->str new-xf))]
    (vec (filter (complement nil?) [k new-props (when content content)]))))

(defmulti rotate-element
  (fn [_ element]
    (first element)))

(defmethod rotate-element :circle
  [deg [k props]]
  (rotate-element-by-transform deg [k props]))

(defmethod rotate-element :ellipse
  [deg [k props]]
  (rotate-element-by-transform deg [k props]))

(defmethod rotate-element :line
  [deg [k props]] 
  (let [pts [[(:x1 props) (:y1 props)] [(:x2 props) (:y2 props)]]
        [[x1 y1] [x2 y2]]  (->> pts
                                (map #(utils/v- % (centroid-of-pts pts)))
                                (map #(rotate-pt deg %))
                                (map #(utils/v+ % (centroid-of-pts pts))))
        new-props (assoc props :x1 x1 :y1 y1 :x2 x2 :y2 y2)]
    [k new-props]))

(defmethod rotate-element :polygon
  [deg [k props]]
  (let [ctr (centroid [k props])
        pts (mapv utils/s->v (str/split (:points props) #" "))
        xpts (->> pts
                  (map #(utils/v- % ctr))
                  (map #(rotate-pt deg %))
                  (map #(utils/v+ % ctr))
                  (map utils/v->s))
        xprops (assoc props :points (apply str (interpose " " xpts)))]
    [k xprops]))

(defmethod rotate-element :polyline
  [deg [k props]]
  (let [ctr (centroid [k props])
        pts (mapv utils/s->v (str/split (:points props) #" "))
        xpts (->> pts
                  (map #(utils/v- % ctr))
                  (map #(rotate-pt deg %))
                  (map #(utils/v+ % ctr))
                  (map utils/v->s))
        xprops (assoc props :points (apply str (interpose " " xpts)))]
    [k xprops]))

(defmethod rotate-element :rect
  [deg [k props]]
  (let [[cx cy] (centroid [k props])
        xf (utils/str->xf-map (get props :transform "rotate(0 0 0)"))
        new-xf (-> xf
                   (update-in [:rotate 0] + deg)
                   (assoc-in  [:rotate 1] cx)
                   (assoc-in  [:rotate 2] cy))
        new-props (assoc props :transform (utils/xf-map->str new-xf))]
    [k new-props]))

(defmethod rotate-element :image
  [deg [k props]]
  (let [[cx cy] (centroid [k props])
        xf (utils/str->xf-map (get props :transform "rotate(0 0 0)"))
        new-xf (-> xf
                   (update-in [:rotate 0] + deg)
                   (assoc-in  [:rotate 1] cx)
                   (assoc-in  [:rotate 2] cy))
        new-props (assoc props :transform (utils/xf-map->str new-xf))]
    [k new-props]))

(defmethod rotate-element :text
  [deg [k props text]]
  (rotate-element-by-transform deg [k props text]))

(defmulti rotate-path-command
  (fn [_ _ m]
    (:command m)))

(defmethod rotate-path-command "M"
  [ctr deg {:keys [:input] :as m}]
  (let [xpt (->> input
                 (#(utils/v- % ctr))
                 (rotate-pt deg)
                 (utils/v+ ctr))]
    (assoc m :input xpt)))

(defmethod rotate-path-command "L"
  [ctr deg {:keys [:input] :as m}]
  (let [xpt (->> input
                 (#(utils/v- % ctr))
                 (rotate-pt deg)
                 (utils/v+ ctr))]
    (assoc m :input xpt)))

(defmethod rotate-path-command "C"
  [ctr deg {:keys [:input] :as m}]
  (let [xinput (->> input
                    (partition 2)
                    (map vec)
                    (map #(utils/v- % ctr))
                    (map #(rotate-pt deg %))
                    (map #(utils/v+ % ctr))
                    (apply concat))]
    (assoc m :input xinput)))

(defmethod rotate-path-command "S"
  [ctr deg {:keys [:input] :as m}]
  (let [xinput (->> input
                    (partition 2)
                    (map vec)
                    (map #(utils/v- % ctr))
                    (map #(rotate-pt deg %))
                    (map #(utils/v+ % ctr))
                    (apply concat))]
    (assoc m :input xinput)))

(defmethod rotate-path-command "Q"
  [ctr deg {:keys [:input] :as m}]
  (let [xinput (->> input
                    (partition 2)
                    (map vec)
                    (map #(utils/v- % ctr))
                    (map #(rotate-pt deg %))
                    (map #(utils/v+ % ctr))
                    (apply concat))]
    (assoc m :input xinput)))

(defmethod rotate-path-command "T"
  [ctr deg {:keys [:input] :as m}]
  (let [xpt (->> input
                 (#(utils/v- % ctr))
                 (rotate-pt deg)
                 (utils/v+ ctr))]
    (assoc m :input xpt)))

;; [rx ry xrot laf swf x y]
;; rx, ry do not change
;; xrot also no change
;; large arc flag and swf again no change
(defmethod rotate-path-command "A"
  [ctr deg {:keys [:input] :as m}]
  (let [[rx ry xrot laf swf ox oy] input
        [nx ny] (->> [ox oy]
                     (#(utils/v- % ctr))
                     (rotate-pt deg)
                     (utils/v+ ctr))]
    (assoc m :input [rx ry (+ xrot deg) laf swf nx ny])))

(defmethod rotate-path-command "Z"
  [_ _ cmd]
  cmd)

(defmethod rotate-path-command :default
  [a cmd]
  [a cmd])

(defmethod rotate-element :path
  [deg [k props]]
  (let [ctr (centroid [k props])
        cmds (path/path-string->commands (:d props))
        xcmds (map #(rotate-path-command ctr deg %) cmds)]
    [k (assoc props :d (path/cmds->path-string xcmds))]))

(declare rotate)
(defmethod rotate-element :g
  [deg [k props & content]]
  (let [[gcx gcy] (centroid-of-pts (bounds (into [k props] content)))
        xfcontent (for [child content]
                    (let [ch (translate [(- gcx) (- gcy)] child)
                          ctr (if (= :g (first ch))
                                (centroid-of-pts (bounds ch))
                                (centroid ch))
                          xfm (->> ctr
                                   (rotate-pt deg)
                                   (utils/v+ [gcx gcy]))]
                      (->> ch
                           (translate (utils/v* [-1 -1] ctr))
                           (rotate deg)
                           (translate-element xfm))))]
    (into [k props] (filter (complement nil?) xfcontent))))

(defn rotate
  "Rotates the `elems` by `deg` around the centroid of the element(s).

  Applied rotations are local."
  [deg & elems]
  (let [elem (first elems)
        elems (rest elems)]
    (when elem
      (cond
        (and (specs/element? elem) (= 0 (count elems)))
        (rotate-element deg elem)
        
        (and (specs/element? elem) (< 0 (count elems)))
        (concat
         [(rotate-element deg elem)]
         [(rotate deg elems)])
        
        :else
        (recur deg (concat elem elems))))))

(defn scale-element-by-transform
  [[sx sy] [k props & content]]
  (let [xf (utils/str->xf-map (:transform props))
        new-xf (-> xf
                   (update :scale (fnil #(map * [sx sy] %) [1 1])))
        new-props (assoc props :transform (utils/xf-map->str new-xf))]
    [k new-props] content))

(defmulti scale-element 
  (fn [_ element]
    (first element)))

;; transforms are applied directly to the properties of shapes.
;; I have scale working the same way. One issue is that scaling a circle
;; turns it into an ellipse. This impl WILL change the shape to ellipse if non-uniform scaling is applied.

(defmethod scale-element :circle
  [[sx sy] [k props]]
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

(defmethod scale-element :ellipse
  [[sx sy] [k props]]
  (let [new-props (-> props
                      (update :rx #(* sx %))
                      (update :ry #(* sy %)))]
    [k new-props]))

;; find bounding box center
;; translate bb-center to 0 0
;; scale all x y values by * [sx sy]
;; translate back to original bb-center

(defmethod scale-element :line
  [[sx sy] [k props]]
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

(defmethod scale-element :polygon
  [[sx sy] [k props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))
        ctr (centroid [k props])
        xpts (->> pts
                  (map (partial scale-pt-from-center ctr [sx sy]))
                  (map utils/v->s))]
    [k (assoc props :points (apply str (interpose " " xpts)))]))

(defmethod scale-element :polyline
  [[sx sy] [k props]]
  (let [pts (mapv utils/s->v (str/split (:points props) #" "))
        ctr (centroid [k props])
        xpts (->> pts
                  (map (partial scale-pt-from-center ctr [sx sy]))
                  (map utils/v->s))]
    [k (assoc props :points (apply str (interpose " " xpts)))]))

(defmethod scale-element :rect
  [[sx sy] [k props]]
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

(defmethod scale-element :image
  [[sx sy] [k props]]
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

(defmethod scale-element :text
  [[sx sy] [k props text]]
  (let [xf (utils/str->xf-map (get props :transform "rotate(0 0 0)"))
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

(defmethod scale-element :g
  [[sx sy] [k props & content]]
  (let [xf (utils/str->xf-map (:transform props))
        new-xf (-> xf
                   (update :scale (fnil #(map * [sx sy] %) [1 1])))
        new-props (assoc props :transform (utils/xf-map->str new-xf))]
    (into [k new-props] content)))

(defmulti scale-path-command
  (fn [_ _ m]
    (:command m)))

(defmethod scale-path-command :default
  [ctr [sx sy] {:keys [:input] :as m}]
  (let [pts (mapv vec (partition 2 input))
        xpts (->> pts
                  (mapcat (partial scale-pt-from-center ctr [sx sy])))]
    (assoc m :input (vec xpts))))

;; this is wrong. just a stub to get moving a bit
(defmethod scale-path-command "A"
  [ctr [sx sy] {:keys [:input] :as m}]
  (let [pts [(take-last 2 input)]
        xpts (->> pts
                  (mapcat (partial scale-pt-from-center ctr [sx sy])))]
    (assoc m :input (vec xpts))))

(defmethod scale-element :path
  [[sx sy] [k props]]
  (let [ctr (centroid [k props])
        cmds (path/path-string->commands (:d props))
        xcmds (map #(scale-path-command ctr [sx sy] %) cmds)]
    [k (assoc props :d (path/cmds->path-string xcmds))]))

(defn scale
  "Scales the `elems` by `sc` about the centroid of the element(s).

  NOTE: this function is still relatively untested and may not behave correctly with group elements."
  [sc & elems]
  (let [[sx sy] (if (coll? sc) sc [sc sc])
        elem (first elems)
        elems (rest elems)]
    (when elem
      (cond
        (and (specs/element? elem) (= 0 (count elems)))
        (scale-element [sx sy] elem)
        
        (and (specs/element? elem) (< 0 (count elems)))
        (concat
         [(scale-element [sx sy] elem)]
         [(scale [sx sy] elems)])
        
        :else
        (recur [sx sy] (concat elem elems))))))
