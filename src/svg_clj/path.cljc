(ns svg-clj.path
  "Provides functions for generating and manipulating SVG path elements.

  Every element provided in [[svg-clj.elements]] has an equivalent function in this namespace that emits path elements with a properly formatted `:d` property.

  The path element has a small Domain Specific Language to create compound shapes and curves. This includes the following commands:

  M = moveto
  L = lineto
  H = horizontal lineto
  V = vertical lineto
  C = curveto
  S = smooth curveto
  Q = quadratic Bézier curve
  T = smooth quadratic Bézier curveto
  A = elliptical Arc
  Z = closepath

  This namespace handles paths by decomposing them into sequences of 'command' maps, which are considered an internal representation; users are not expected to construct paths using commands."
  (:require [clojure.string :as str]
            [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.parametric :as p]))

(defn path
  "Wraps a path string `d` in a hiccup-style data structure.

  The path string is assumed to already be a valid path string. Users should use path generating functions provided in this namespace for constructing paths in the same manor as the other renderable SVG elements.

  More complex paths can be built by combining paths with the function `merge-paths`"
  [d]
  [:path {:d d :fill-rule "evenodd"}])

(defn- any-vh?
  [cmds]
  (seq (filter #{"H" "V"} (map :command cmds))))

(defn- convert-vh
  [[pcmd ccmd]]
  (if (and (not (any-vh? [pcmd])) ;;prev. cmd must NOT be VH
           (any-vh? [ccmd])) ;; curr. cmd must be VH
    (let [[px py] (take-last 2 (:input pcmd))
          {:keys [command input coordsys]} ccmd
          xinput (cond
                   (and (= command "H") (= coordsys :abs)) [(first input) py]
                   (and (= command "V") (= coordsys :abs)) [px (first input)]
                   (and (= command "H") (= coordsys :rel)) [(+ (first input) px) py]
                   (and (= command "V") (= coordsys :rel)) [px (+ (first input) py)])
          ncmd (-> ccmd
                   (assoc :command "L")
                   (assoc :coordsys :abs)
                   (assoc :input xinput))]
      [pcmd ncmd])
    [pcmd ccmd]))

(defn- convert-first-vh-cmd
  [cmds]
  (let [icmd (first cmds)]
    (cons icmd 
          (->> cmds
               (partition 2 1)
               (map convert-vh)
               (map second)))))

(defn vh->l
  "Converts any v (vertical) or h (horizontal) commands into l (line) commands.
  This is necessary to allow rotation of a path element, because rotating an axis-aligned line will move it off the axis, making it unrepresentable with v or h commands, as they do not encode the x or y position values respectively."
  [cmds]
  (let [iters (iterate convert-first-vh-cmd cmds)]
    (if (any-vh? cmds)
      (->> iters
           (partition 2 1)
           (take-while (fn [[a b]] (not= a b)))
           last
           last)
      cmds)))

(defn- any-rel?
  [cmds]
  (seq (filter #{:rel} (map :coordsys cmds))))

(defn- convert-rel
  [[pcmd ccmd]]
  (if (and (= :abs (:coordsys pcmd))
           (= :rel (:coordsys ccmd)))
    (let [{:keys [command input]} ccmd
          abs-cursor (vec (take-last 2 (:input pcmd)))
          xinput (if (= command "A")
                   (vec (concat
                         (drop-last 2 input)
                         (utils/v+ (take-last 2 input) abs-cursor)))
                   (vec (mapcat #(utils/v+ % abs-cursor) (partition 2 input))))
          ncmd (-> ccmd
                   (assoc :coordsys :abs)
                   (assoc :input xinput)
                   (assoc :cursor abs-cursor))]
      [pcmd ncmd])
    [pcmd ccmd]))

(defn- convert-first-rel
  [cmds]
  (let [icmd (first cmds)]
    (cons icmd 
          (->> cmds
               (partition 2 1)
               (map convert-rel)
               (map second)))))

(defn rel->abs
  "Converts any relative coordinate commands into absoulte coordinate commands."
  [cmds]
  (let [iters (iterate convert-first-rel cmds)]
    (if (any-rel? cmds)
      (->> iters
           (partition 2 1)
           (take-while (fn [[a b]] (not= a b)))
           last
           last)
      cmds)))

(defn- any-t?
  [cmds]
  (seq (filter #{"T"} (map :command cmds))))

(defn- convert-t
  [[pcmd ccmd]]
  (if (and (= "Q" (:command pcmd))
           (= "T" (:command ccmd)))
    (let [[cpt pt] (partition 2 (:input pcmd))
          ncpt (utils/rotate-pt-around-center cpt 180.0 pt)
          {:keys [command input coordsys]} ccmd
          xinput (vec (concat ncpt input))
          ncmd (-> ccmd
                   (assoc :command "Q")
                   (assoc :input xinput))]
      [pcmd ncmd])
    [pcmd ccmd]))

(defn- convert-first-t-cmd
  [cmds]
  (let [icmd (first cmds)]
    (cons icmd 
          (->> cmds
               (partition 2 1)
               (map convert-t)
               (map second)))))

(defn t->q
  "Converts any T curve commands into Q curve commands."
  [cmds]
  (let [iters (iterate convert-first-t-cmd cmds)]
    (if (any-t? cmds)
      (->> iters
           (partition 2 1)
           (take-while (fn [[a b]] (not= a b)))
           last
           last)
      cmds)))

(defn- any-s?
  [cmds]
  (seq (filter #{"S"} (map :command cmds))))

(defn- convert-s
  [[pcmd ccmd]]
  (if (and (= "C" (:command pcmd))
           (= "S" (:command ccmd)))
    (let [[_ cpt pt] (partition 2 (:input pcmd))
          ncpt (utils/rotate-pt-around-center cpt 180.0 pt)
          {:keys [command input coordsys]} ccmd
          xinput (vec (concat ncpt input))
          ncmd (-> ccmd
                   (assoc :command "C")
                   (assoc :input xinput))]
      [pcmd ncmd])
    [pcmd ccmd]))

(defn- convert-first-s-cmd
  [cmds]
  (let [icmd (first cmds)]
    (cons icmd 
          (->> cmds
               (partition 2 1)
               (map convert-s)
               (map second)))))

(defn s->c
  "Converts any S curve commands into C curve commands."
  [cmds]
  (let [iters (iterate convert-first-s-cmd cmds)]
    (if (any-s? cmds)
      (->> iters
           (partition 2 1)
           (take-while (fn [[a b]] (not= a b)))
           last
           last)
      cmds)))

(defn- path-cmd-strs
  "Split the path string `ps` into a vector of path command strings."
  [ps]
  (-> ps
      (str/replace #"\n" " ")
      (str/split #"(?=[A-DF-Za-df-z])")
      (#(map str/trim %))
      (#(filter (complement empty?) %))))

(defn- relative?
  "Returns true if the path command string `cs` has a relative coordinate command.
  Relative coordinate commands are lowercase in the `d` property string.
  Absolute coordinate commands are uppercase in the `d` property string."
  [cs]
  (let [csx (first (str/split cs #"[a-df-z]"))]
    (not (= cs csx))))

(defn- coord-sys-key
  "Returns the command string `cs`'s coord. system key.
  Key is either :rel or :abs."
  [cs]
  (if (relative? cs) :rel :abs))

(defn- cmd-input
  [cs]
  (let [i (str/split cs #"[A-DF-Za-df-z]")]
    (when (seq (rest i))
      (apply utils/s->v (rest i)))))

(defn- cmd-str->cmd
  "Transforms a command string `cs` into a map."
  [cs]
  {:command  (str/upper-case (re-find #"[A-DF-Za-df-z]" cs))
   :coordsys (coord-sys-key cs)
   :input (cmd-input cs)})

(defn- merge-cursor
  [[pcmd ccmd]]
  (let [cursor (vec (take-last 2 (:input pcmd)))]
    (assoc ccmd :cursor cursor)))

(defn path-str->cmds
  "Turns path string `ps` into a list of its command maps."
  [ps]
  (->> ps
       path-cmd-strs
       (map cmd-str->cmd)
       (concat [{:command "M"
                 :coordsys :abs
                 :input [0 0]}])
       (partition 2 1)
       (map merge-cursor)
       vh->l
       rel->abs
       t->q
       s->c))

(defn- cmd->path-string
  [{:keys [:command :coordsys :input]}]
  (let [c (if (= coordsys :abs)
            command
            (str/lower-case command))]
    (str c (str/join " " input))))

(defn cmds->path-string
  "Generates a valid string for the path element `:d` property from a list of command maps `cmds`."
  [cmds]
  (let [start (first cmds)
        cmds (if (= "M" (:command start))
               cmds
               (let [new-start {:command "M"
                                :coordsys :abs
                                :input (:cursor start)
                                :cursor [0 0]}]
                 (concat [new-start] cmds)))]
    (when (> (count cmds) 1)
      (str/join " " (map cmd->path-string cmds)))))

(defn- pt->l
  [pt]
  {:command "L"
   :coordsys :abs
   :input (vec pt)})

(defn- pt->m
  [pt]
  {:command "M"
   :coordsys :abs
   :input (vec pt)})

(defn bezier
  "Emits a path element with a bezier curve defined by the control points `a`, `b`, `c`, and sometimes `d`.
   Quadratic curves use 3 control points, and cubic curves use 4 control points."
  ([a b c]
   (let [open (pt->m a)]
     (-> {:command "Q"
          :coordsys :abs
          :input (concat b c)}
         list
         (conj open)
         vec
         cmds->path-string
         path)))

  ([a b c d]
   (let [open (pt->m a)]
     (-> {:command "C"
          :coordsys :abs
          :input (concat b c d)}
         list
         (conj open)
         vec
         cmds->path-string
         path))))

(defn- build-arc
  [rx ry rot laf sw a b]
  (let [open (pt->m a)]
    (-> {:command "A"
         :coordsys :abs
         :input (concat [rx ry rot laf sw] b)}
        list
        (conj open)
        vec
        cmds->path-string
        path)))

(defn arc
  "Emits a path element with an arc starting at `pt-a` and ending at a point rotated by degrees, `deg`, around `ctr` in the counter-clockwise direction."
  [pt-a ctr deg]
  (let [r (utils/distance pt-a ctr)
        angle 0
        b (utils/rotate-pt-around-center pt-a deg ctr)
        laf (if (<= deg 180) 0 1)]
     (build-arc r r angle laf 1 pt-a b)))

(defn circle
  "Emits a circle using two arcs in a path element with radius `r` centered at the origin."
  [r]
  (let [open (pt->m [r 0])
        close {:command "Z"
               :coordsys :abs
               :input nil}]
    (-> [open
         {:command "A"
          :coordsys :abs
          :input [r r 0 1 0 0 r]}
         {:command "A"
          :coordsys :abs
          :input [r r 0 1 0 (- r) 0]}
         {:command "A"
          :coordsys :abs
          :input [r r 0 1 0 0 (- r)]}
         {:command "A"
          :coordsys :abs
          :input [r r 0 1 0 r 0]}
         close]
        cmds->path-string
        path)))

(defn ellipse
  "Emits an ellipse element with x-axis radius `rx` and y-axis radius `ry` centered at the origin."
  [rx ry]
  (let [open (pt->m [rx 0])
        close {:command "Z"
               :coordsys :abs
               :input nil}]
    (-> [open
         {:command "A"
          :coordsys :abs
          :input [rx ry 0 1 0 0 ry]}
         {:command "A"
          :coordsys :abs
          :input [rx ry 0 1 0 (- rx) 0]}
         {:command "A"
          :coordsys :abs
          :input [rx ry 0 1 0 0 (- ry)]}
         {:command "A"
          :coordsys :abs
          :input [rx ry 0 1 0 rx 0]}
         close]
        cmds->path-string
        path)))

(defn line
  "Emits a line using a path element starting at 2d point `pt-a` and ending at 2d point `pt-b`."
  [pt-a pt-b]
  (-> [(pt->m pt-a) (pt->l pt-b)]
      cmds->path-string
      path))

(defn polygon
  "Emits a polygon using a path element with 2d points from vector or list `pts`.
  Polygons use a closed path."
  [pts]
  (let [open (pt->m (first pts))
        close {:command "Z"
               :coordsys :abs
               :input nil}]
    (-> (map pt->l (rest pts))
        (conj open)
        vec
        (conj close)
        cmds->path-string
        path)))

(defn polyline
  "Emits a polyline using a path element with 2d points from vector or list `pts`.
  Polylines use an open path."
  [pts]
  (let [open (pt->m (first pts))]
    (-> (map pt->l (rest pts))
        (conj open)
        vec
        cmds->path-string
        path)))

(defn rect
  "Emits a rectangle using a path element of width `w` and height `h` centered at the origin."
  [w h]
  (let [w2 (/ w 2.0)
        h2 (/ h 2.0)]
    (polygon [ [(- w2) (- h2)] [w2 (- h2)] 
               [w2 h2]          [(- w2) h2] ])))

(defmulti cmd->pts :command)

(defmethod cmd->pts :default
  [{:keys [input]}]
  (mapv vec (partition 2 input)))

;; this is not implemented correctly yet.
;; just a 'stub' returning the end point of the arc
(defmethod cmd->pts "A"
  [{:keys [input cursor]}]
  (let [[rx ry deg laf sw x y] input
        b [x y]
        ctr (utils/v+ cursor [rx 0])
        sa (utils/angle-from-pts cursor ctr b)
        angle (if (= 1 laf) (- 360 sa) sa)
        mids (mapv #(utils/rotate-pt-around-center cursor % ctr) (rest (range 0 angle 90)))]
    [cursor b] #_(conj mids b)))

(defn centroid
  [[_ props]]
  (let [cmds (path-str->cmds (:d props))
        pts (mapcat cmd->pts cmds)]
    (utils/centroid-of-pts (vec (into #{} pts)))))

(defn bounds
  [[_ props]]
  (let [cmds (path-str->cmds (:d props))
        pts (mapcat cmd->pts cmds)]
    (utils/bounds-of-pts pts)))

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
  [{:keys [:input] :as m} [x _]]
  (assoc m :input (utils/v+ [x] input)))

(defmethod translate-path-command "V"
  [{:keys [:input] :as m} [_ y]]
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

(defn translate
  [[k props] [x y]]
  (let [cmds (path-str->cmds (:d props))
        xcmds (map #(translate-path-command % [x y]) cmds)]
    [k (assoc props :d (cmds->path-string xcmds))]))

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

(defn rotate
  [[k props] deg]
  (let [ctr (centroid [k props])
        cmds (path-str->cmds (:d props))
        xcmds (map #(rotate-path-command % ctr deg) cmds)]
    [k (assoc props :d (cmds->path-string xcmds))]))

(defmulti scale-path-command
  (fn [cmd _ _]
    (:command cmd)))

(defmethod scale-path-command :default
  [{:keys [:input] :as m} ctr [sx sy]]
  (let [pts (mapv vec (partition 2 input))
        xpts (->> pts
                  (mapcat (partial utils/scale-pt-from-center ctr [sx sy])))]
    (assoc m :input (vec xpts))))

;; this is wrong. just a stub to get moving a bit
(defmethod scale-path-command "A"
  [{:keys [:input] :as m} ctr [sx sy]]
  (let [pts [(take-last 2 input)]
        xpts (->> pts
                  (mapcat (partial utils/scale-pt-from-center ctr [sx sy])))]
    (assoc m :input (vec xpts))))

(defn scale
  [[k props] [sx sy]]
  (let [ctr (centroid [k props])
        cmds (path-str->cmds (:d props))
        xcmds (map #(scale-path-command % ctr [sx sy]) cmds)]
    [k (assoc props :d (cmds->path-string xcmds))]))

(defn split-path
  "Splits a single path element containing multiple disjoint paths into a group of paths containing only one path."
  [[k props]]
  (let [ps (-> (:d props)
               (str/split #"(?=M)")
               (->> (map str/trim)))]
    (map #(assoc-in [k props] [1 :d] %) ps)))

(defn explode-path
  "Breaks a path element into its constituent curves.
  Optional arg `break-polys?` is `false` by default, which treats sequences of line segments as polylines.
  Setting `break-polys?` to `true` treats sequences of line segments as individual elements."
  [[_ {:keys [d]}] & {:keys [break-polys?]}]
  (let [break-fn (if break-polys?
                   (partial partition 1)
                   (partial partition-by :command))]
    (->> d
         path-str->cmds
         vh->l
         break-fn
         (map cmds->path-string)
         (filter some?)
         (map path))))

(defn- bezier-cmd-pts
  [{:keys [input cursor]}]
  (let [control-pts (partition 2 (concat cursor input))
        c (p/bezier control-pts)]
    (map c (range 0 1.05 0.05))))

(defn- cmds->elements
  [cmds]
  (let [start (first cmds)
        cmds (if (= "M" (:command start))
               cmds
               (let [new-start {:command "M"
                                :coordsys :abs
                                :input (:cursor start)
                                :cursor [0 0]}]
                 (concat [new-start] cmds)))]
    (when (> (count cmds) 1)
      (let [cs (map :command (rest cmds))]
        (cond
          ;; empty
          (and (= (count cmds) 2)
               (empty? (remove #{"Z"} cs)))
          nil

          ;; circle or ellipse
          (and (= (count cmds) 6)
               (empty? (remove #{"A" "Z"} cs)))
          (let [eps 0.00001
                [rx ry] (take 2 (:input (second cmds)))
                [x y] (take-last 2 (:input (second cmds)))
                [cx cy] (utils/v* [1.0 1.0] (utils/centroid-of-pts (set (mapcat cmd->pts cmds))))]
            (if (< (Math/abs (- rx ry)) eps)
              [:circle {:cx cx :cy cy :r rx}]
              [:ellipse {:cx cx :cy cy :rx rx :ry ry}]))

          ;; line
          (and (= (count cmds) 2)
               (empty? (remove #{"L"} cs)))
          (apply el/line (map :input cmds))

          ;; polyline
          (and (> (count cmds) 2)
               (empty? (remove #{"L"} cs)))
          (el/polyline (map :input cmds))

          ;; polygon
          (and (> (count cmds) 2)
               (empty? (remove #{"L" "Z"} cs)))
          (el/polygon (map :input cmds))

          ;; Quadratic or Cubic Bezier Curve(s)
          (or (empty? (remove #{"Q"} cs))
              (empty? (remove #{"C"} cs)))
          (let [pts (mapcat bezier-cmd-pts (rest cmds))]
            (el/polyline pts))

          ;; Quadratic or Cubic Bezier Curve(s) closed path
          (or (empty? (remove #{"Q" "Z"} cs))
              (empty? (remove #{"C" "Z"} cs)))
          (let [pts (mapcat bezier-cmd-pts (drop-last (rest cmds)))]
            (el/polygon pts))
          
          :else
          (path (cmds->path-string cmds)))))))

(defn- clean-m-cmds-threshold
  "Remove cmdb if it is an M command with the same position as the last input of cmda."
  [[cmda cmdb]]
  (let [merge-dist 1.0
        [pa pb] (map (comp (partial take-last 2) :input) [cmda cmdb])
        [ca cb] (map :command [cmda cmdb])]
    (cond
      (= "M" ca) [] ;; discard M in first position always
      (and (< (utils/distance pa pb) merge-dist) (= "M" cb)) [cmda]
      :else [cmda cmdb])))

(defn- clean-m-cmds
  "Remove cmdb if it is an M command with the same position as the last input of cmda."
  [[cmda cmdb]]
  (let [[pa pb] (map (comp (partial take-last 2) :input) [cmda cmdb])
        [ca cb] (map :command [cmda cmdb])]
    (cond
      (= "M" ca) [] ;; discard M in first position always
      (and (= pa pb) (= "M" cb)) [cmda]
      :else [cmda cmdb])))

(defn merge-paths
  "Merges a list of path elements together, keeping props from last path in the list."
  [& paths]
  (let [[_ props] (last paths)
        cmds (mapcat #(path-str->cmds (get-in % [1 :d])) paths)
        xf-cmds
        (conj 
         (remove nil? (mapcat clean-m-cmds-threshold (partition 2 1 (rest cmds))))
         (first cmds))]
    [:path (assoc props :d (cmds->path-string xf-cmds))]))

(defn- get-subpaths
  [cmds]
  (->> cmds
       (partition-by  #((complement #{"M"}) (:command %)))
       (partition 2)
       (map #(apply concat %))))

(defn- subpath->elements
  [cmds]
  (let [split-path (partition-by :command cmds)
        cmd-check (into #{} (map #(:command (first %)) split-path))]
    (if (or (= cmd-check #{"M" "L" "Z"})
            (= cmd-check #{"M" "A" "Z"})
            (= cmd-check #{"M" "L"}))
      (cmds->elements cmds)
      (let [subpath (->> cmds
                         (remove #(#{"M" "Z"} (:command %)))
                         (partition-by :command)
                         (map cmds->elements)
                         (remove nil?))]
        (apply el/g
         (conj
          (vec subpath)
          (when (= "Z" (:command (last cmds)))
            (let [[s e] (map #(take-last 2 (:input %))
                             [(first cmds) (last (drop-last cmds))])]
              (el/line s e)))))))))

(defn path->elements
  [[_ {:keys [d]}]]
  (->> d
       path-str->cmds
       get-subpaths
       (map subpath->elements)
       (remove nil?)))

(defmulti element->path
  (fn [element]
    (if (keyword? (first element))
      (first element)
      :list)))

(defmethod element->path :list
  [elems]
  (map element->path elems))

(defmethod element->path :circle
  [[_ {:keys [cx cy r] :as props}]]
  (-> (circle r)
      (translate [cx cy])
      (utils/style (dissoc props :cx :cy :r))))

(defmethod element->path :ellipse
  [[_ {:keys [cx cy rx ry] :as props}]]
  (-> (ellipse rx ry)
      (translate [cx cy])
      (utils/style (dissoc props :cx :cy :rx :ry))))

(defmethod element->path :rect
  [[_ {:keys [width height x y] :as props}]]
  (let [ctr (utils/v+ [x y] [(/ width 2.0) (/ height 2.0)])]
    (-> (rect width height)
        (translate ctr)
        (utils/style (dissoc props :width :height :x :y)))))

(defmethod element->path :line
  [[_ {:keys [x1 y1 x2 y2] :as props}]]
  (-> (line [x1 y1] [x2 y2])
      (utils/style (dissoc props :x1 :y1 :x2 :y2))))

(defmethod element->path :polyline
  [[_ {:keys [points] :as props}]]
  (let [pts (partition 2 (utils/s->v points))]
    (-> (polyline pts)
        (utils/style (dissoc props :points)))))

(defmethod element->path :polygon
  [[_ {:keys [points] :as props}]]
  (let [pts (partition 2 (utils/s->v points))]
    (-> (polygon pts)
        (utils/style (dissoc props :points)))))

(defmethod element->path :path
  [elem]
  elem)

(defn- needs-closing?
  [path]
  (let [cmds (path-str->cmds (get-in path [1 :d]))
        start (->> cmds first :input (take-last 2))
        end (->> cmds last :input (take-last 2))]
    (= start end)))

(defmethod element->path :g
  [[_ props & elems]]
  (let [p (apply merge-paths (map element->path elems))]
    (if (needs-closing? p)
      (->> elems
           drop-last
           (map element->path)
           (apply merge-paths)
           (#(utils/style % props)))
      (-> p
          (utils/style props)))))

(defn elements->path
  [elems]
  (apply merge-paths (map element->path elems)))

(defn decurve
  [path]
  (->> (path->elements path)
       (map element->path)
       (apply merge-paths)))
