<img src="https://github.com/adam-james-v/svg-clj/blob/main/examples/quilt.png" alt="A colourful render of a quilt design" width="300">

# svg-clj

[![Clojars Project](https://img.shields.io/clojars/v/io.github.adam-james-v/svg-clj.svg)](https://clojars.org/io.github.adam-james-v/svg-clj)

svg-clj is a library for creating SVGs in Clojure/Clojurescript. This is done using functions which emit hiccup data structures. Since hiccup is quite common and well-known in the Clojure ecosystem, it is rather simple to use svg-clj alongside other libraries that emit and/or expect hiccup-style data structures.

You can try things out in the browser here:
[svg-clj-interactive](https://adam-james-v.github.io/svg-clj-interactive/index.html)

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
;; emitted hiccup works with Reagent as well.

(tools/save-svg blossom "examples/blossom.svg")
```

Other examples, also available in the examples directory of this project:

- [basics](https://adam-james-v.github.io/svg-clj/examples/basics)
- [layout](https://adam-james-v.github.io/svg-clj/examples/layout)
- [bezier](https://adam-james-v.github.io/svg-clj/examples/bezier)
- [offset](https://adam-james-v.github.io/svg-clj/examples/offset)

## Known Issues
There are several features I have yet to completely fix.

- scale transformation can have confusing behaviour when scaled elements are used in groups.
- Bounds and Centroid calculations can give incorrect results with paths containing arcs or bezier curves
- text elements have only basic support (translate, rotate, style work). Other transforms have buggy workarounds for the fact that text element dimensions are not known until rasterization
- offset works for all elements except paths, which is a WIP feature
- arc implementation is still buggy, particularly when rotating.

## Using svg-clj in your project

If you use lein or boot, place this in your project.clj:

```clj
[io.github.adam-james-v/svg-clj "0.0.1-SNAPSHOT"]
```

If you use Clojure's CLI, add this to your :deps in deps.edn:


```clj
io.github.adam-james-v/svg-clj {:mvn/version "0.0.1-SNAPSHOT"}
```

NOTE: check that you're grabbing the version you want.

## Design
The library uses hiccup syntax to represent the SVG diagrams being created. The user writes functional code to define various elements of the SVG and has access to transformations via utility functions.

Since the library functions emit hiccup data structures, the user can extend and manipulate their data using other clojure libraries or their own functions. 

The library has two main categories of functions:

- elements
  - container elements (svg, figure...)
  - shapes
    - circle
    - ellipse
    - line
    - path
    - polygon
    - polyline
    - rect
  - text
  - g
  - composites (custom functions using shapes.. eg. arrow)
  
- transforms and property calcs
  - centroid
  - bounds
  - rotate
  - translate
  - scale
  - style
  - offset
  - explode paths
  - merge paths

### Explaining the namespaces
I've annotated a ns declaration to help make sense of where you can find various functions.

```clj
(ns examples.blossom
  (:require
    ;; math helpers, simple data manip helpers
    [svg-clj.utils :as utils]
    
    ;; all of the shape functions like rect, circle, polygon, etc.
    [svg-clj.elements :as el]
    
    ;; all of the transforms, including path specific fns
    [svg-clj.transforms :as tf]
    
    ;; shapes built from other shapes, AND the svg container fn
    [svg-clj.composites :as comp :refer [svg]]
    
    ;; draw elements using path instead, and has the 'commands' path DSL
    ;; also has arc and bezier drawing fns
    [svg-clj.path :as path]

    ;; parametric curve fns and point list generators useful for layouts
    [svg-clj.parametric :as p]

    ;; layout functions like distribute-linear and distribute-along-curve
    [svg-clj.layout :as lo]

    ;; when in CLJ context, use cider-show, show, save-svg, load-svg
    ;; to help with the dev. process
    #?(:clj [svg-clj.tools :as tools])))
```

Every transform takes an element or list of elements, performs the appropriate actions, and returns an element or list of elements with the transform 'baked in' to the properties.

For example, a circle begins as follows:

```clj
(el/circle 50)
;; => [:circle {:cx 0, :cy 0, :r 50}]

(tf/translate (el/circle 50) [25 25])
;; => [:circle {:cx 25, :cy 25, :r 50, :transform "rotate(0 25 25)"}]
```

Notice how the circle's cx and cy properties have changed according to the transformation.

An important thing to consider with this approach is that it is 'lossy' in some sense. The user's design intent is clear when reading the source they provide, but is lost when compiled to SVG. The call to the translate function is not explicit in the output. This may not be the behaviour everyone expects, so just be aware of this if you need to pass the output to another program or perhaps to another person.

## Opinionated Approach
This is not quite a straight wrapper for SVG functionality. I have altered the default behavior of some functions.

For example, a rectangle is drawn centered around the orgin by default. Plain SVG rectangles draw with the first corner located at the origin by default.

All rotations are applied to shapes locally by default. This means that a circle at [10 0] rotated by 90 deg will not appear to move using svg-clj; the shape itself is being spun around it's center, but that center point is not moving. Default SVG behaviour rotates around the origin by default. So, any elements offset from the orgin will move large distances away from their starting positions. 

This choice was made because it feels more intuitive (to me, at least) to draw with local transformation operations in mind.

## Threading
Greencoder (one of my Twitch viewers) sent several twitter DMs with some criticisms/feedback. All have been appropriately addressed, but I wanted to highlight his thoughts regarding my use of threading macros. 

"thread last macro should be kept for stream operations to compose better with other fns. I think that translate-element should take elem as first argument."
- GreenCoder (Twitch handle)

strictly speaking, translate and rotate are not operating on streams of data, but rather on objects
 - assoc and dissoc use thread first. That is, you do a thing to a single 'object'
 - map and filter use thread last, and are expected to work on all types of seq-able things lists... lazy, infinite

So, to keep the mental model the same, I have designed my transform fns to always take the element being transformed as the first arg. I find threading to be a very readable and intuitive way to 'build up' transforms on some basic element. You can see this approach throughout my various examples.


## Further Reading

If you would like to understand my motivations, decisions, and reasoning for the choices I've made in this library, you can read the .org file in the top level of this repo.

[svg-clj.org](https://github.com/adam-james-v/svg-clj/blob/main/svg-clj.org).

I use a 'freehand' literate programming style in my org files. This just means that I have a scattered approach. Please be patient if you're reading the notes; they may not always make sense or have full context.

Proper documentation is, naturally, a key element in bringing this project from prototype to release.

## Other Work (That I've heard of so far)

[Dali](https://github.com/stathissideris/dali) is a library by Stathis Sideris that also works with SVG. Since I have only recently heard about this library, I have not yet had time to do a detailed comparison but, at a glance, some differences I see are:

| svg-clj                                | dali                                            |
|:--------------------------------------:|:-----------------------------------------------:|
| write functions which emit hiccup data | write hiccup data directly                      |
| very basic layout engine               | layout engine is a key feature                  |
| no built-in rasterization              | rasterize SVGs using Batik                      |
| Clojure and Clojurescript              | Clojure only                                    |
| SVG primitives only (for now)          | SVG primitives + 'prefabs' (eg. markers/arrows) |
