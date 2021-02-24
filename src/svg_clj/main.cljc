(ns svg-clj.main
  (:require [clojure.string :as st]
            [clojure.spec.alpha :as s]
            #?(:clj [clojure.data.xml :as xml])
            [svg-clj.utils :as utils]
            [svg-clj.specs :as specs]
            [svg-clj.transforms :as transforms]
            [svg-clj.path :as path]))

;; add these fns into the main ns for API purposes.
;; Not sure of a better way to do this yet.
(defn centroid
  "Calculates the arithmetic mean position of all points of all given `elems`."
  [& elems]
  (apply transforms/centroid elems))

(defn bounds
  "Calculates the axis-aligned bounding box of `elems`.
  The returned bounding box is a list of four points:
  [Bottom Left, Bottom Right, Top Right, Top Left]."
  [& elems]
  (apply transforms/bounds elems))

(defn translate 
  "Translates the `elems` by `x` and `y` relative to the element(s)'s current position(s).

  For example, a shape sitting at [10 10] being translated by [10 10] will be located at [20 20] after translation."
  [[x y] & elems]
  (apply #(transforms/translate [x y] %) elems))

(defn rotate
  "Rotates the `elems` by `deg` around the centroid of the element(s).

  Applied rotations are local."
  [deg & elems]
  (apply #(transforms/rotate deg %) elems))

(defn scale
  "Scales the `elems` by `sc` about the centroid of the element(s).

  NOTE: this function is still relatively untested and may not behave correctly with group elements."
  [sc & elems]
  (apply #(transforms/scale sc %) elems))

(defn path
  "Wraps a path string `d` in a hiccup-style data structure.
  
  The path string is minimally evaluated and is otherwise untouched. Users should consider the function `polygon-path` for constructing paths from points. More complex paths can be built by combining paths with the function `merge-paths`"
  [d]
  (path/path d))

(defn merge-paths
  "Merges a list of path elements together, keeping props from the last path in the list."
  [& paths]
  (apply path/merge-paths paths))

(defn polygon-path
  "Creates a path element from the list of 2d points `pts`."
  [pts]
  (path/polygon-path pts))

(defn svg
  "This fn wraps `content` in an SVG container element.
  The SVG container is parameterized by width `w`, height `h`, and scale `sc`."
  [[w h sc] & content]
  [:svg {:width  w
         :height h
         :viewBox (str "0 0 " w " " h)
         :xmlns "http://www.w3.org/2000/svg"}
   [:g {:transform (str "scale(" sc ")")} content]])

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
  {:pre [(specs/pt2d? [ax ay]) (specs/pt2d? [bx by])]}
  [:line {:x1 ax :y1 ay :x2 bx :y2 by}])

(defn polygon
  [pts]
  {:pre [(specs/pts? pts)]}
  [:polygon {:points (apply str (interpose " " (map utils/v->s pts)))}])

(defn polyline
  [pts]
  {:pre [(specs/pts? pts)]}
  [:polyline {:points (apply str (interpose " " (map utils/v->s pts)))}])

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

#?(:clj
   (defn xml->hiccup
     [xml]
     (if-let [t (:tag xml)]
       (let [elem [t]
             elem (if-let [attrs (:attrs xml)]
                    (conj elem attrs)
                    elem)]
         (into elem (map xml->hiccup (:content xml))))
       xml)))

#?(:clj
   (defn ->edn
     [str]
     (->> (xml/parse-str str 
                         :skip-whitespace true
                         :namespace-aware false)
          xml->hiccup
          #_(tree-seq vector? rest)
          #_(filter vector?)
          #_(filter #(= :svg (first %)))
          #_first)))

#?(:clj
   (defn unwrap-elements
     [edn]
     (filter specs/element? edn)))

(defn style
  [style [k props & content]]
  (into [k (merge props style)] content))
