(ns examples.layout
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [hiccup.core :refer [html]]
            [svg-clj.composites :as cp :refer [svg]]
            [svg-clj.utils :as utils]
            [svg-clj.elements :as svg]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.layout :as lo]
            #?(:clj [svg-clj.tools :as tools])))

(defn show-debug-geom
  [elem]
  (let [ctr (tf/centroid elem)
        bds (tf/bounds elem)]
    (svg/g elem
       (svg/g (-> (svg/polygon bds)
               (tf/style {:fill "none"
                       :stroke "red"
                       :stroke-width "3px"}))
          (-> (svg/circle 2)
               (tf/translate ctr)
               (tf/style {:fill "red"}))))))

(defn rand-rect
  []
  (-> (svg/rect (+ 5 (rand-int 20)) (+ 5 (rand-int 20)))
      (tf/style {:fill (str "rgb("
                            (rand-int 255) ","
                            (rand-int 255) ","
                            (rand-int 255) ")")})))

(tools/cider-show (map show-debug-geom (drop 2 (lo/distribute-linear :x 10 (repeatedly 7 rand-rect)))))

(tools/cider-show (map show-debug-geom (drop 2 (lo/distribute-linear :y 10 (repeatedly 7 rand-rect)))))

#_(tools/cider-show 
 (lo/distribute-on-pts 
  (repeatedly rand-rect)
  (lo/rect-grid 10 10 30 30)))


(def txt "(defn cider-show
  [svg-data]
  (let [fname \"_imgtmp.png\"
        data (if (= (first svg-data) :svg)
               svg-data
               (svg svg-data))]
    (do (png! data fname)
        (clojure.java.io/file fname))))")

(def theme
  { 0 "#2e3440" ;; main bg col
    1 "#3b4252"
    2 "#434c5e"
    3 "#4c566a"
    4 "#d8dee9"  ;; main col for text
    5 "#e5e9f0"
    6 "#eceff4"
    7 "#8fbcbb"
    8 "#88c0d0"
    9 "#81a1c1"
   10 "#5e81ac"
   11 "#bf616a"
   12 "#d08770"
   13 "#ebcb8b"
   14 "#a3be8c"
   15 "#b48ead"})

(def symbol-map
  {"(" (get theme 3)
   ")" (get theme 3)
   "def" (get theme 9)
   "n" (get theme 9)  ;; cheat for def/defn matching
   "let" (get theme 9)
   "if" (get theme 9)
   "do" (get theme 9)})

(defn colorize
  [txt symbol-map theme-map]
  (let [f (fn [txt]
            (let [col (get symbol-map txt)]
              (if col
                [:tspan {:fill col} txt]
                txt)))
        ts (-> txt
               (str/replace #"\(" ";(;")
               (str/replace #"\)" ";);")
               (str/replace #"defn" ";defn;")
               (str/replace #"def" ";def;")
               (str/replace #"let" ";let;")
               (str/replace #"if" ";if;")
               #_(str/replace #"do" ";do;")
               (str/split #";"))]
    (map f ts)))

(defn codeblock-dims
  [txt font-size]
  (let [lines (str/split txt #"\n")
        nlines (count lines)
        ncols (apply max (map count lines))]
    [(* (dec ncols) (int (* font-size 2/3)))
     (int (* nlines (+ font-size 3.25)))]))

(defn codeblock
  [txt font-size symbol-map theme-map]
  (let [[w h] (codeblock-dims txt 16)
        ts (-> txt
               (str/split #"\n")
               (->> (map #(colorize % symbol-map theme-map))))
        f (fn [txt]
            (-> (svg/text txt)
                (tf/style {:xml:space "preserve"
                           :font-family "Menlo"
                           :text-anchor "start"
                           :fill (get theme-map 6)
                           :font-size font-size})))
        elems (mapv f ts)]
    (-> (for [i (range (count ts))]
          (-> (get elems i)
              (tf/translate [0 (* i (+ font-size 3))])))
        (tf/translate [0 font-size]))))

(defn code-window
  [txt font-size symbol-map theme-map]
  (let [pad 20
        [w h] (utils/v+ (repeat 2 (* 2 pad)) (codeblock-dims txt font-size))]
  (svg/g
   ;; bg
   (-> (svg/rect (+ w 100) (+ h 100))
       (tf/style {:fill (get theme-map 14)}))
   ;; window 'shadow'
   (-> (svg/rect w h)
       (tf/translate [4 4])
       (tf/style {:rx 9
                  :opacity 0.2
                  :fill "black"}))
   (-> (svg/rect w h)
       (tf/translate [2 2])
       (tf/style {:rx 9
                  :opacity 0.2
                  :fill "black"}))
   ;; window bg
   (-> (svg/rect w h)
       (tf/style {:rx 9
                  :fill (get theme-map 0)}))
   (-> (codeblock txt font-size symbol-map theme-map)
       (tf/translate [(+ pad (/ w -2.0)) (+ (* 0.80 pad) (/ h -2.0))])))))

(defn- anim-frames! [f name framerate dur]
  (let [mkdir (sh "mkdir" "-pv" name)
        frames (int (* framerate dur))
        framefn (fn [fr] (tools/png!
                          (f (/ fr frames))
                          (format (str name "/%03d.png") fr)))]
    (when (= 0 (:exit mkdir))
      (into [] (map framefn (range 1 (inc frames)))))))

(defn- anim-video! [name framerate]
  (let [ffmpeg 
        (sh "ffmpeg" "-f" "image2" "-r" (str framerate)
            "-i" (str name "/%03d.png")
            "-c:v" "libvpx-vp9" "-vf" "format=rgba"
            "-pix_fmt" "yuva420p" "-b:v" "800k"
            "-y" (str name ".webm"))]
    (when (= 0 (:exit ffmpeg))
      (sh "rm" "-rf" name))))

(defn animate! [{:keys [graphics-fn name framerate duration]}]
  (do (anim-frames! graphics-fn name framerate duration)
      (anim-video! name framerate)))


(def anim
  {:name "type"
   :framerate 30
   :duration 7
   :graphics-fn
   (fn [t]
     (let [txt (clojure.repl/source-fn 'lo/distribute-linear)
           i (int (* t (count txt)))
           blank (str/replace txt #"[^\n]" " ")
           ntxt (concat (take i txt) (drop i blank))]
       (svg (code-window (apply str ntxt) 16 symbol-map theme))))})

(tools/cider-show 
 (lo/distribute-on-curve
  (repeatedly 40 rand-rect)
  (lo/p-circle 150)))

(tools/cider-show 
 (lo/distribute-linear
  :x
  20
  (repeatedly 10 rand-rect)))
