# svg-clj

[![Clojars Project](https://img.shields.io/clojars/v/io.github.adam-james-v/svg-clj.svg)](https://clojars.org/io.github.adam-james-v/svg-clj)

svg-clj is a library for creating SVGs in Clojure/Clojurescript. This is done using functions which emit hiccup data structures. Since hiccup is quite common and well-known in the Clojure ecosystem, it is rather simple to use svg-clj alongside other libraries that emit and/or expect hiccup-style data structures.

Here is an example using most of svg-clj's features:

![An SVG Image of a stylized flower blossom.](https://github.com/adam-james-v/svg-clj/blob/main/examples/blossom.svg "Blossom")

This blossom is produced with the following code:

```clojure
(ns examples.blossom
  (:require [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.transforms :as tf]
            [svg-clj.composites :as comp :refer [svg]]
            [svg-clj.path :as path]
            [svg-clj.parametric :as p]
            [svg-clj.layout :as lo]
            [svg-clj.tools :as tools]
            #?(:clj [svg-clj.tools :as tools])))
            
(defn flip-y
  [pts]
  (mapv #(utils/v* % [1 -1]) pts))

(defn petal
  [cpts]
  (let [beza (apply path/bezier cpts)
        bezb (apply path/bezier (flip-y cpts))
        shape (tf/merge-paths beza bezb)
        ctr (tf/centroid shape)]
    (-> shape
        (tf/rotate -90)
        (tf/translate (utils/v* ctr [-1 -1])))))

(defn petal-ring
  [petal r n]
  (el/g
   (lo/distribute-on-curve
    (repeat n petal)
    (p/circle r))))

(def petal-01
  (-> (petal [[0 0] [5 -50] [50 -20] [75 0]])
      (tf/style {:fill "#ff8b94"
                 :stroke "#ffaaa5"
                 :stroke-width "4px"
                 :stroke-linecap "round"})))

(def petal-02
  (-> (petal [[0 0] [1 -20] [20 -10] [40 0]])
      (tf/style {:fill "#ffaaa5"
                 :stroke "none"})))

(def petal-03
  (-> (tf/merge-paths petal-01 petal-02)
      (tf/style {:fill "#a8e6cf"})))

(def petal-ring-01 (petal-ring petal-01 120 12))
(def petal-ring-02 (petal-ring petal-02 120 12))

(def petal-ring-03
  (-> (petal-ring petal-03 70 6)
      (tf/rotate (/ 360.0 24))))

(def petal-ring-04
  (let [petal (-> petal-03 (tf/style {:fill "#cc5963"}))]
    (-> (petal-ring petal 90 6)
        (tf/rotate (/ 360.0 24))
        (tf/rotate (/ 360.0 12)))))

(def petal-ring-05
  (let [petal (-> petal-02
                  (tf/rotate 180)
                  (tf/style {:fill "none"
                             :stroke "#f4f1d7"
                             :stroke-width "2px"}))]
    (-> (petal-ring petal 70 36)
        (tf/rotate (/ 360.0 24)))))

(def petal-ring-06
  (let [petal (-> petal-02
                  (tf/style {:fill "none"
                             :stroke "#f4f1d7"
                             :stroke-width "2px"}))]
    (-> (petal-ring petal 40 20)
        (tf/rotate (/ 360.0 24)))))

(def blossom (el/g
              (-> (el/circle 105) (tf/style {:fill "#69b599"}))
              petal-ring-01
              petal-ring-02
              petal-ring-06
              petal-ring-05
              petal-ring-04
              petal-ring-03))

;; when in a Clojure context, you can compile to SVG files
;; this uses the Hiccup html compiler
;; emitted SVG data works with Reagent as well.

(tools/save-svg blossom "examples/blossom.svg")
```

## deps.edn

```clj
svg-clj/svg-clj {:git/url "https://github.com/adam-james-v/svg-clj"
                 :sha "grab-latest-sha"}
```


If you would like to understand my motivations, decisions, and reasoning for the choices I've made in this library, you can read the .org file in the top level of this repo.

[svg-clj.org](https://github.com/adam-james-v/svg-clj/blob/main/svg-clj.org). I attempt to do literate programming in my org files, but I have a scattered approach, so please be patient if you're reading the notes; they may not always make sense. Proper documentation is, naturally, a key element in bringing this project from prototype to release.

## Other Work

[Dali](https://github.com/stathissideris/dali) is a library by Stathis Sideris that also works with SVG. Since I have only recently heard about this library, I have not yet had time to do a detailed comparison but, at a glance, some differences I see are:

|                    svg-clj                   |                       dali                      |
|:--------------------------------------------:|:-----------------------------------------------:|
| users write functions which emit hiccup data | users write hiccup data directly                |
| very basic layout engine                     | layout engine is a key feature                  |
| no built-in rasterization                    | rasterize SVGs using Batik                      |
| Clojure and Clojurescript                    | Clojure only                                    |
| SVG primitives only (for now)                | SVG primitives + 'prefabs' (eg. markers/arrows) |
