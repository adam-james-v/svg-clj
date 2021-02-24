(ns svg-clj.main
  (:require [clojure.string :as st]
            [clojure.spec.alpha :as s]
            [clojure.pprint :as pp]
            [clojure.data.xml :as xml]
            #?(:clj
               [hiccup.core :refer [html]])
            #?(:cljs
               [cljs.reader :refer [read-string]])))

(defn svg
  "This function wraps `content` in an SVG container element.
  The SVG container is parameterized by width `w`, height `h`, and scale `sc`."
  [[w h sc] & content]
  [:svg {:width  w
         :height h
         :viewBox (str "0 0 " w " " h)
         :xmlns "http://www.w3.org/2000/svg"}
   [:g {:transform (str "scale(" sc ")")} content]])

;; vector arithmetic helpers
(def v+ (partial mapv +))
(def v- (partial mapv -))
(def v* (partial mapv *))

;; simple calcs
(defn to-deg
  [rad]
  (* rad (/ 180 Math/PI)))

(defn to-rad
  [deg]
  (* deg (/ Math/PI 180)))

(defn round
  [num places]
  (let [d (Math/pow 10 places)]
    (/ (Math/round (* num d)) d)))

(defn average
  [& numbers]
  (let [n (count numbers)]
    (/ (apply + numbers) n)))

;; some string transformation tools
(defn v->s
  "Turns the vector `v` into a string formatted for use in SVG attributes."
  [v]
  (apply str (interpose "," v)))

(defn s->v
  "Turns a string of comma or space separated numbers into a vector."
  [s]
  (-> s
      (st/trim)
      (st/split #"[, ]")
      (#(filter (complement empty?) %))
      (#(mapv read-string %))))

(defn xf-kv->str
  [[k v]]
  (str (symbol k) (apply list v)))

(defn str->xf-kv
  [s]
  (let [split (st/split s #"\(")
        key (keyword (first split))
        val (vec (read-string (str "(" (second split))))]
    [key val]))

(defn xf-map->str
  [m]
  (apply str (interpose "\n" (map xf-kv->str m))))

(defn str->xf-map
  [s]
  (if-let [s s]
    (into {} (map str->xf-kv (st/split-lines s)))
    {}))

(s/def ::pt2d (s/tuple number? number?))
(s/def ::pts (s/coll-of ::pt2d))

(def svg-element-keys
  "SVG elements provided by the library."
  #{:circle
    :ellipse
    :line
    :path
    :polygon
    :polyline
    :rect
    :text
    :image
    :g})

(s/def ::basic-element
  (s/cat :tag svg-element-keys
         :props map?))

(s/def ::text-element
  (s/cat :tag #{:text}
         :props map?
         :content string?))

(s/def ::g-element
  (s/cat :tag #{:g}
         :props map?
         :content (s/* ::svg-element)))

(s/def ::svg-element
  (s/or :basic (s/spec ::basic-element)
        :text (s/spec ::text-element)
        :group (s/spec ::g-element)))

(s/def ::path-element
  (s/cat :tag #{:path}
         :props (s/keys :req-un [::d]) 
         :content (s/* ::svg-element)))

#_(s/def ::groupable
  (s/or :flat (s/* (s/spec ::svg-element))
        :nested (s/cat :a (s/* (s/spec ::svg-element)))))
(s/def ::groupable
  (s/or :flat (s/every ::svg-element)
        :nested (s/coll-of (s/every ::svg-element))))

(defn pt2d? [a] (s/valid? ::pt2d a))
(defn pts? [s] (s/valid? ::pts s))

(defn element?
  "Checks if `elem` is an SVG element."
  [elem]
  (s/valid? ::svg-element elem))

(defn path-string-allowed? 
  [string] 
  (empty? (st/replace string #"[MmZzLlHhVvCcSsQqTtAaeE0-9-,.\s]" "")))

(defn path-string-valid-syntax?
  [string]
  (nil? (re-find #"[a-zA-Z][a-zA-Z]" string)))

(defn path-string-valid-start?
  [string]
  (nil? (re-find #"^[0-9-,.]" string)))

(defn path-string-valid-end?
  [string]
  (nil? (re-find #".*[-,.]$" string)))

(defn path-string-single-command?
  [string]
  (= 1 (count (re-seq #"[A-DF-Za-df-z]" string))))

(s/def ::path-string
  (s/and string?
         (complement empty?)
         path-string-allowed?
         path-string-valid-syntax?
         path-string-valid-start?
         path-string-valid-end?
         (complement path-string-single-command?)))

(s/def ::command-string
  (s/and string?
         (complement empty?)
         path-string-allowed?
         path-string-valid-syntax?
         path-string-valid-start?
         path-string-valid-end?
         path-string-single-command?))

(def commands #{"M" "L" "H" "V" "C" "S" "Q" "T" "A" "Z"})
(s/def ::command commands)
(s/def ::coordsys #{:rel :abs})
(s/def ::input (s/or :data (s/+ number?)
                     :nil nil?))
(s/def ::command-map
  (s/keys :req-un [::command ::coordsys ::input]))

(s/def ::bounds
  (s/tuple ::pt2d ::pt2d ::pt2d ::pt2d))

(defn circle
  [r]
  {:pre [(number? r)]}
  [:circle {:cx 0 :cy 0 :r r}])

(defn ellipse
  [rx ry]
  {:pre [(number? rx) (number? ry)]}
  [:ellipse {:cx 0 :cy 0 :rx rx :ry ry}])

(defn line
  [[ax ay] [bx by]]
  {:pre [(pt2d? [ax ay]) (pt2d? [bx by])]}
  [:line {:x1 ax :y1 ay :x2 bx :y2 by}])

(defn polygon
  [pts]
  {:pre [(pts? pts)]}
  [:polygon {:points (apply str (interpose " " (map v->s pts)))}])

(defn polyline
  [pts]
  {:pre [(pts? pts)]}
  [:polyline {:points (apply str (interpose " " (map v->s pts)))}])

(defn rect
  [w h]
  {:pre [(number? w) (number? h)]}
  [:rect {:width w :height h :x (/ w -2.0) :y (/ h -2.0)}])

(defn image
  [url w h]
  {:pre [(string? url) (number? w) (number? h)]}
  [:image {:href url :width w :height h :x (/ w -2.0) :y (/ h -2.0)}])

(defn g
  [& content]
  {:pre [(s/valid? ::groupable content)]}
  (if (and (= 1 (count content))
           (not (keyword? (first (first content)))))
    ;; content is a list of a list of elements
    (into [:g {}] (first content))
    ;; content is a single element OR a list of elements
    (into [:g {}] (filter (complement nil?) content))))

(defn text
  [text]
  {:pre [(string? text)]}
  [:text {:x 0 :y 0} text])

(defn path
  [d]
  {:pre [(s/valid? ::path-string d)]}
  [:path {:d d
          :fill-rule "evenodd"}])

(defn- path-command-strings
  "Split the path string `ps` into a vector of path command strings."
  [ps]
  {:pre [(s/valid? ::path-string ps)]}
  (-> ps
      (st/replace #"\n" " ")
      (st/split #"(?=[A-DF-Za-df-z])")
      (#(map st/trim %))
      (#(filter (complement empty?) %))))

(defn- relative?
  "True if the path command string `cs` has a relative coordinate command.
  Relative coordinate commands are lowercase.
  Absolute coordinate commands are uppercase."
  [cs]
  {:pre [(s/valid? ::command-string cs)]}
  (let [csx (first (st/split cs #"[a-z]"))]
    (not (= cs csx))))

(defn- coord-sys-key
  "Returns the command string `cs`'s coord. system key.
  Key is either :rel or :abs."
  [cs]
  {:pre [(s/valid? ::command-string cs)]}
  (if (relative? cs) :rel :abs))

(defn- command-input
  [cs]
  {:pre [(s/valid? ::command-string cs)]}
  (let [i (st/split cs #"[A-DF-Za-df-z]")]
    (when (not (empty? (rest i)))
      (apply s->v (rest i)))))

(defn- command
  "Transforms a command string `cs` into a map."
  [cs]
  {:pre [(s/valid? ::command-string cs)]}
  {:command  (st/upper-case (re-find #"[A-DF-Za-df-z]" cs))
   :coordsys (coord-sys-key cs)
   :input (command-input cs)})

(defn- path-string->commands
  "Turns path string `ps` into a list of its command maps."
  [ps]
  {:pre [(s/valid? ::path-string ps)]}
  (->> ps
       (path-command-strings)
       (map command)))

(defn- any-vh?
  [cmds]
  {:pre [(s/valid? (s/coll-of ::command-map) cmds)]}
  (not (empty? (filter #{:vline :hline} (map :command cmds)))))

(defn- convert-vh
  [[pcmd ccmd]]
  {:pre [(s/valid? ::command-map pcmd)
         (s/valid? ::command-map ccmd)]}
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
  {:pre [(s/valid? (s/coll-of ::command-map) cmds)]}
  (let [icmd (first cmds)]
  (cons icmd 
        (->> cmds
             (partition 2 1)
             (map convert-vh)
             (map second)))))

(defn- vh->l
  [cmds]
  {:pre [(s/valid? (s/coll-of ::command-map) cmds)]}
  (let [iters (iterate convert-first-vh-cmd cmds)]
    (->> iters
         (partition 2 1)
         (take-while (fn [[a b]] (not= a b)))
         last
         last)))

(defn- cmd->path-string
  [{:keys [:command :coordsys :input] :as cmd}]
  {:pre [(s/valid? ::command-map cmd)]}
  (let [c (if (= coordsys :abs)
            command
            (st/lower-case command))]
    (str c (apply str (interpose " " input)))))

(defn- cmds->path-string
  [cmds]
  {:pre [(s/valid? (s/coll-of ::command-map) cmds)]}
  (apply str (interpose " " (map cmd->path-string cmds))))

(defn merge-paths
  "Merges a list of path elements together, keeping props from last path in the list."
  [& paths]
  {:pre [(s/valid? (s/coll-of ::path-element) paths)]}
  (let [props (second (last paths))
        d (apply str (interpose " " (map #(get-in % [1 :d]) paths)))]
    [:path (assoc props :d d)]))

(defn- pt->l
  [pt]
  {:pre [(s/valid? ::pt2d pt)]}
  {:command "L"
   :coordsys :abs
   :input (vec pt)})

(defn- pt->m
  [pt]
  {:pre [(s/valid? ::pt2d pt)]}
  {:command "M"
   :coordsys :abs
   :input (vec pt)})

(defn polygon-path
  [pts]
  {:pre [(s/valid? ::pts pts)]}
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

(defn centroid-of-pts
  "Calculates the arithmetic mean position of all the given `pts`."
  [pts]
  {:pre [(s/valid? ::pts pts)]}
  (let [ndim (count (first (sort-by count pts)))
        splits (for [axis (range 0 ndim)]
                 (map #(nth % axis) pts))]
    (mapv #(apply average %) splits)))

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
  (let [pts (mapv s->v (st/split (:points props) #" "))]
    (centroid-of-pts pts)))

(defmethod centroid-element :polyline
  [[_ props]]
  (let [pts (mapv s->v (st/split (:points props) #" "))]
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

(defmulti command->pts :command)

(defmethod command->pts :default
  [{:keys [:input]}]
  (mapv vec (partition 2 input)))

;; this is not implemented correctly yet.
;; just a 'stub' returning the end point of the arc
(defmethod command->pts "A"
  [{:keys [:input]}]
  [(vec (take-last 2 input))])

(defmethod centroid-element :path
  [[_ props]]
  (let [cmds (path-string->commands (:d props))
        pts (mapcat command->pts cmds)]
    (centroid-of-pts pts)))

(declare centroid)
(defmethod centroid-element :g
  [[_ props & content]]
  (centroid-of-pts (into #{} (map centroid content))))

(defn centroid
  [& elems]
  {:pre [(s/valid? ::groupable elems)]}
  (if (and (= 1 (count elems))
           (not (keyword? (first (first elems)))))
    ;; content is a list of a list of elements
    (recur (first elems))
    ;; content is a single element OR a list of elements
    (centroid-of-pts (mapv centroid-element elems))))

(defn pts->bounds
  [pts]
  {:pre [(s/valid? ::pts pts)]}
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
        pts (mapv #(v+ c %) [[r 0]
                             [0 r]
                             [(- r) 0]
                             [0 (- r)]])]
    (pts->bounds pts)))

(declare rotate-pt-around-center)
(defmethod bounds-element :ellipse
  [[_ props]]
  (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        deg (get-in xf [:rotate 0])
        mx (get-in xf [:rotate 1])
        my (get-in xf [:rotate 2])
        c [(:cx props) (:cy props)]
        rx (:rx props)
        ry (:ry props)
        pts (mapv #(v+ c %) [[rx 0]
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
  (let [pts (mapv s->v (st/split (:points props) #" "))]
    (pts->bounds pts)))

(defmethod bounds-element :polyline
  [[_ props]]
  (let [pts (mapv s->v (st/split (:points props) #" "))]
    (pts->bounds pts)))

(defmethod bounds-element :rect
  [[_ props]]
  (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
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
  (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
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
  (let [cmds (path-string->commands (:d props))
        pts (mapcat command->pts cmds)]
    (pts->bounds pts)))

(declare bounds)
(defmethod bounds-element :g
  [[_ props & content]]
  (pts->bounds (apply concat (map bounds content))))

(defn bounds
  [& elems]
  {:pre [(s/valid? ::groupable elems)]}
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
  (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        cx (:cx props)
        cy (:cy props)
        new-xf (-> xf
                   (assoc-in [:rotate 1] (+ x cx))
                   (assoc-in [:rotate 2] (+ y cy)))
        new-props (-> props
                      (assoc :transform (xf-map->str new-xf))
                      (update :cx + x)
                      (update :cy + y))]
    [k new-props]))

(defmethod translate-element :ellipse
  [[x y] [k props]]
  (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        cx (:cx props)
        cy (:cy props)
        new-xf (-> xf
                   (assoc-in [:rotate 1] (+ x cx))
                   (assoc-in [:rotate 2] (+ y cy)))
        new-props (-> props
                      (assoc :transform (xf-map->str new-xf))
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
  (let [pts (mapv s->v (st/split (:points props) #" "))
        xpts (->> pts 
                  (map (partial v+ [x y]))
                  (map v->s))]
    [k (assoc props :points (apply str (interpose " " xpts)))]))

(defmethod translate-element :polyline
  [[x y] [k props]]
  (let [pts (mapv s->v (st/split (:points props) #" "))
        xpts (->> pts 
                  (map (partial v+ [x y]))
                  (map v->s))]
    [k (assoc props :points (apply str (interpose " " xpts)))]))

(defmethod translate-element :rect
  [[x y] [k props]]
  (let [[cx cy] (centroid [k props])
        xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        new-xf (-> xf
                   (assoc-in [:rotate 1] (+ cx x))
                   (assoc-in [:rotate 2] (+ cy y)))
        new-props (-> props
                      (assoc :transform (xf-map->str new-xf))
                      (update :x + x)
                      (update :y + y))]
    [k new-props]))

(defmethod translate-element :image
  [[x y] [k props]]
  (let [[cx cy] (centroid [k props])
        xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        new-xf (-> xf
                   (assoc-in [:rotate 1] (+ cx x))
                   (assoc-in [:rotate 2] (+ cy y)))
        new-props (-> props
                      (assoc :transform (xf-map->str new-xf))
                      (update :x + x)
                      (update :y + y))]
    [k new-props]))

(defmethod translate-element :text
  [[x y] [k props text]]
  (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        new-xf (-> xf
                   (update-in [:rotate 1] + x)
                   (update-in [:rotate 2] + y))
        new-props (-> props
                      (assoc :transform (xf-map->str new-xf))
                      (update :x + x)
                      (update :y + y))]
    [k new-props text]))

(defmulti translate-path-command
  (fn [_ m]
    (:command m)))

(defmethod translate-path-command "M"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (v+ [x y] input)))

(defmethod translate-path-command "L"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (v+ [x y] input)))

(defmethod translate-path-command "H"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (v+ [x] input)))

(defmethod translate-path-command "V"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (v+ [y] input)))

;; x y x y x y because input will ahve the form:
;; [x1 y1 x2 y2 x y] (first two pairs are control points)
(defmethod translate-path-command "C"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (v+ [x y x y x y] input)))

;; similar approach to above, but one control point is implicit
(defmethod translate-path-command "S"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (v+ [x y x y] input)))

(defmethod translate-path-command "Q"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (v+ [x y x y] input)))

(defmethod translate-path-command "T"
  [[x y] {:keys [:input] :as m}]
  (assoc m :input (v+ [x y] input)))

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
  (let [cmds (path-string->commands (:d props))
        xcmds (map #(translate-path-command [x y] %) cmds)]
    [k (assoc props :d (cmds->path-string xcmds))]))

(declare translate)
(defmethod translate-element :g
  [[x y] [k props & content]]
  (->> content
       (map (partial translate [x y]))
       (filter (complement nil?))
       (into [k props])))

(defn translate
  [[x y] & elems]
  {:pre [(s/valid?
          (s/or :one (s/coll-of ::svg-element)
                :many (s/cat :elems (s/coll-of ::svg-element))) elems)
         (s/valid? ::pt2d [x y])]}
  (let [elem (first elems)
        elems (rest elems)]
    (when elem
      (cond
        (and (element? elem) (= 0 (count elems)))
        (translate-element [x y] elem)
        
        (and (element? elem) (< 0 (count elems)))
        (concat
         [(translate-element [x y] elem)]
         [(translate [x y] elems)])
      
        :else
        (recur [x y] (concat elem elems))))))

(defn rotate-element-by-transform
  [deg [k props content]]
  (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        new-xf (-> xf
                   (update-in [:rotate 0] + deg))
        new-props (assoc props :transform (xf-map->str new-xf))]
    (vec (filter (complement nil?) [k new-props (when content content)]))))

(defn move-pt
  [mv pt]
  (v+ pt mv))

(defn rotate-pt
  [deg [x y]]
  (let [c (Math/cos (to-rad deg))
        s (Math/sin (to-rad deg))]
    [(- (* x c) (* y s))
     (+ (* x s) (* y c))]))

(defmulti rotate-element
  (fn [_ element]
    (first element)))

(defmethod rotate-element :circle
  [deg [k props]]
  (rotate-element-by-transform deg [k props]))

(defmethod rotate-element :ellipse
  [deg [k props]]
  (rotate-element-by-transform deg [k props]))

(defn rotate-pt-around-center
  [deg center pt]
  (->> pt
       (move-pt (map - center))
       (rotate-pt deg)
       (move-pt center)))

(defmethod rotate-element :line
  [deg [k props]] 
  (let [pts [[(:x1 props) (:y1 props)] [(:x2 props) (:y2 props)]]
        [[x1 y1] [x2 y2]]  (->> pts
                                (map #(v- % (centroid-of-pts pts)))
                                (map #(rotate-pt deg %))
                                (map #(v+ % (centroid-of-pts pts))))
        new-props (assoc props :x1 x1 :y1 y1 :x2 x2 :y2 y2)]
    [k new-props]))

(defmethod rotate-element :polygon
  [deg [k props]]
  (let [ctr (centroid [k props])
        pts (mapv s->v (st/split (:points props) #" "))
        xpts (->> pts
                  (map #(v- % ctr))
                  (map #(rotate-pt deg %))
                  (map #(v+ % ctr))
                  (map v->s))
        xprops (assoc props :points (apply str (interpose " " xpts)))]
    [k xprops]))

(defmethod rotate-element :polyline
  [deg [k props]]
  (let [ctr (centroid [k props])
        pts (mapv s->v (st/split (:points props) #" "))
        xpts (->> pts
                  (map #(v- % ctr))
                  (map #(rotate-pt deg %))
                  (map #(v+ % ctr))
                  (map v->s))
        xprops (assoc props :points (apply str (interpose " " xpts)))]
    [k xprops]))

(defmethod rotate-element :rect
  [deg [k props]]
  (let [[cx cy] (centroid [k props])
        xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        new-xf (-> xf
                   (update-in [:rotate 0] + deg)
                   (assoc-in  [:rotate 1] cx)
                   (assoc-in  [:rotate 2] cy))
        new-props (assoc props :transform (xf-map->str new-xf))]
    [k new-props]))

(defmethod rotate-element :image
  [deg [k props]]
  (let [[cx cy] (centroid [k props])
        xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        new-xf (-> xf
                   (update-in [:rotate 0] + deg)
                   (assoc-in  [:rotate 1] cx)
                   (assoc-in  [:rotate 2] cy))
        new-props (assoc props :transform (xf-map->str new-xf))]
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
                 (#(v- % ctr))
                 (rotate-pt deg)
                 (v+ ctr))]
    (assoc m :input xpt)))

(defmethod rotate-path-command "L"
  [ctr deg {:keys [:input] :as m}]
  (let [xpt (->> input
                 (#(v- % ctr))
                 (rotate-pt deg)
                 (v+ ctr))]
    (assoc m :input xpt)))

(defmethod rotate-path-command "C"
  [ctr deg {:keys [:input] :as m}]
  (let [xinput (->> input
                    (partition 2)
                    (map vec)
                    (map #(v- % ctr))
                    (map #(rotate-pt deg %))
                    (map #(v+ % ctr))
                    (apply concat))]
    (assoc m :input xinput)))

(defmethod rotate-path-command "S"
  [ctr deg {:keys [:input] :as m}]
  (let [xinput (->> input
                    (partition 2)
                    (map vec)
                    (map #(v- % ctr))
                    (map #(rotate-pt deg %))
                    (map #(v+ % ctr))
                    (apply concat))]
    (assoc m :input xinput)))

(defmethod rotate-path-command "Q"
  [ctr deg {:keys [:input] :as m}]
  (let [xinput (->> input
                    (partition 2)
                    (map vec)
                    (map #(v- % ctr))
                    (map #(rotate-pt deg %))
                    (map #(v+ % ctr))
                    (apply concat))]
    (assoc m :input xinput)))

(defmethod rotate-path-command "T"
  [ctr deg {:keys [:input] :as m}]
  (let [xpt (->> input
                 (#(v- % ctr))
                 (rotate-pt deg)
                 (v+ ctr))]
    (assoc m :input xpt)))

;; [rx ry xrot laf swf x y]
;; rx, ry do not change
;; xrot also no change
;; large arc flag and swf again no change
(defmethod rotate-path-command "A"
  [ctr deg {:keys [:input] :as m}]
  (let [[rx ry xrot laf swf ox oy] input
        [nx ny] (->> [ox oy]
                     (#(v- % ctr))
                     (rotate-pt deg)
                     (v+ ctr))]
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
        cmds (path-string->commands (:d props))
        xcmds (map #(rotate-path-command ctr deg %) cmds)]
    [k (assoc props :d (cmds->path-string xcmds))]))

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
                                   (v+ [gcx gcy]))]
                      (->> ch
                           (translate (v* [-1 -1] ctr))
                           (rotate deg)
                           (translate-element xfm))))]
    (into [k props] (filter (complement nil?) xfcontent))))

(defn asdf
  [deg [k props & content]]
  (let [[gcx gcy] (centroid-of-pts (bounds (into [k props] content)))
        xfcontent (for [child content]
                    #_child
                    (let [ch (translate [(- gcx) (- gcy)] child)
                          ctr (if (= :g (first ch))
                                (centroid-of-pts (bounds ch))
                                (centroid ch))
                          xfm (->> ctr
                                   (rotate-pt deg)
                                   (v+ [gcx gcy]))]
                      (->> ch
                           (translate (v* [-1 -1] ctr))
                           (rotate deg)
                           (translate xfm))))]
    xfcontent #_(into [k props] (filter (complement nil?) xfcontent))))

(defn rotate
  [deg & elems]
  (let [elem (first elems)
        elems (rest elems)]
    (when elem
      (cond
        (and (element? elem) (= 0 (count elems)))
        (rotate-element deg elem)
        
        (and (element? elem) (< 0 (count elems)))
        (concat
         [(rotate-element deg elem)]
         [(rotate deg elems)])
        
        :else
        (recur deg (concat elem elems))))))

(defn scale-element-by-transform
  [[sx sy] [k props & content]]
  (let [xf (str->xf-map (:transform props))
        new-xf (-> xf
                   (update :scale (fnil #(map * [sx sy] %) [1 1])))
        new-props (assoc props :transform (xf-map->str new-xf))]
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
  (let [pts (mapv s->v (st/split (:points props) #" "))
        ctr (centroid [k props])
        xpts (->> pts
                  (map (partial scale-pt-from-center ctr [sx sy]))
                  (map v->s))]
    [k (assoc props :points (apply str (interpose " " xpts)))]))

(defmethod scale-element :polyline
  [[sx sy] [k props]]
  (let [pts (mapv s->v (st/split (:points props) #" "))
        ctr (centroid [k props])
        xpts (->> pts
                  (map (partial scale-pt-from-center ctr [sx sy]))
                  (map v->s))]
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
  (let [xf (str->xf-map (get props :transform "rotate(0 0 0)"))
        cx (get-in xf [:rotate 1])
        cy (get-in xf [:rotate 2])
        x (+ (* (- (:x props) cx) sx) cx)
        y (+ (* (- (:y props) cy) sy) cy)
        new-xf (-> xf
                   (assoc-in [:rotate 1] (- x))
                   (assoc-in [:rotate 2] (- y)))
        new-props (-> props
                      (assoc :transform (xf-map->str new-xf))
                      (assoc :x x)
                      (assoc :y y)
                      (update-in [:style :font-size] #(* % sx)))]
    [k new-props text]))

(defmethod scale-element :g
  [[sx sy] [k props & content]]
  (let [xf (str->xf-map (:transform props))
        new-xf (-> xf
                   (update :scale (fnil #(map * [sx sy] %) [1 1])))
        new-props (assoc props :transform (xf-map->str new-xf))]
    (into [k new-props] content)))

#_(defmethod scale-element :path
  [[sx sy] [k props]]
  (let [path-strings (st/split-lines (:d props))
        paths (map path-string->path path-strings)
        center (f/bb-center-2d (apply concat (map :pts paths)))
        new-paths (for [path paths]
                    (let [xf (partial scale-pt-from-center center [sx sy])
                          xpts (map xf (:pts path))]
                      (path->path-string (assoc path :pts xpts))))
        new-props (assoc props :d (apply str (interpose "\n" new-paths)))]
    [k new-props]))

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
        cmds (path-string->commands (:d props))
        xcmds (map #(scale-path-command ctr [sx sy] %) cmds)]
    [k (assoc props :d (cmds->path-string xcmds))]))

(defn scale
  [sc & elems]
  (let [[sx sy] (if (coll? sc) sc [sc sc])
        elem (first elems)
        elems (rest elems)]
    (when elem
      (cond
        (and (element? elem) (= 0 (count elems)))
        (scale-element [sx sy] elem)
        
        (and (element? elem) (< 0 (count elems)))
        (concat
         [(scale-element [sx sy] elem)]
         [(scale [sx sy] elems)])
        
        :else
        (recur [sx sy] (concat elem elems))))))

(defn style
  [style [k props & content]]
  (into [k (merge props style)] content))

(defn xml->hiccup
  [xml]
  (if-let [t (:tag xml)]
    (let [elem [t]
          elem (if-let [attrs (:attrs xml)]
                 (conj elem attrs)
                 elem)]
      (into elem (map xml->hiccup (:content xml))))
    xml))

(defn ->edn
  [str]
  (->> (xml/parse-str str 
                      :skip-whitespace true
                      :namespace-aware false)
       xml->hiccup
       #_(tree-seq vector? rest)
       #_(filter vector?)
       #_(filter #(= :svg (first %)))
       #_first))

(defn unwrap-elements
  [edn]
  (filter element? edn))
