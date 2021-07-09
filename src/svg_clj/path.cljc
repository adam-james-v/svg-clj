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
            [svg-clj.utils :as utils]))

(defn path
  "Wraps a path string `d` in a hiccup-style data structure.

  The path string is assumed to already be a valid path string. Users should use path generating functions provided in this namespace for constructing paths in the same manor as the other renderable SVG elements.

  More complex paths can be built by combining paths with the function `merge-paths`"
  [d]
  [:path {:d d :fill-rule "evenodd"}])

(defn- path-command-strings
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
  (let [csx (first (str/split cs #"[a-z]"))]
    (not (= cs csx))))

(defn- coord-sys-key
  "Returns the command string `cs`'s coord. system key.
  Key is either :rel or :abs."
  [cs]
  (if (relative? cs) :rel :abs))

(defn- command-input
  [cs]
  (let [i (str/split cs #"[A-DF-Za-df-z]")]
    (when (seq (rest i))
      (apply utils/s->v (rest i)))))

(defn- command
  "Transforms a command string `cs` into a map."
  [cs]
  {:command  (str/upper-case (re-find #"[A-DF-Za-df-z]" cs))
   :coordsys (coord-sys-key cs)
   :input (command-input cs)})

(defn- merge-cursor
  [[pcmd ccmd]]
  (let [cursor (vec (take-last 2 (:input pcmd)))]
    (assoc ccmd :cursor cursor)))

(defn path-string->commands
  "Turns path string `ps` into a list of its command maps."
  [ps]
  (->> ps
       path-command-strings
       (map command)
       (concat [{:command "M"
                 :coordsys :abs
                 :input [0 0]}])
       (partition 2 1)
       (map merge-cursor)))

(defn- any-vh?
  [cmds]
  (seq (filter #{:vline :hline} (map :command cmds))))

(defn- convert-vh
  [[pcmd ccmd]]
  (if (and (not (any-vh? [pcmd])) ;;prev. cmd must NOT be VH
           (any-vh? [ccmd])) ;; curr. cmd must be VH
    (let [[px py] (take-last 2 (:input pcmd))
          vh (:command ccmd)
          xinput (cond (= vh :hline) [(first (:input ccmd)) py]
                       (= vh :vline) [px (first (:input ccmd))])
          ncmd (-> ccmd
                   (assoc :command :line)
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
          :input [r r 0 1 0 (- r) 0]}
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
          :input [rx ry 0 1 0 (- rx) 0]}
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
