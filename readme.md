# svg-clj

A helper library for creating SVGs in Clojure/Clojurescript.

You can play around with the library [here](https://adam-james-v.github.io/dev/svg-clj/)

This is currently a work in progress. Until a stable release is provided, this library is considered to be in a 'prototype' state. Breaking changes are possible until a proper release is achieved.

![An SVG Image of Circles Spiralling, shrinking, and fading towards the image center.](https://github.com/adam-james-v/svg-clj/blob/main/examples/circles.svg "Circles")

These circles are produced by the following code:

```clojure
(ns example
  (:require [svg-clj.main :as svg]
            [svg-clj.transforms :refer [rotate-pt]]
            [hiccup.core :refer [html]]))

(def circles
  (svg/svg
   [200 200 1]
   (->>
    (svg/g 
      (for [a (range 0 12)]
        (->> (svg/circle (+ 5 (* a 4)))
             (svg/translate [(/ (+ 5 (* a 4)) 2) 0])
             (svg/translate (rotate-pt (* a -40) [20 0]))
             (svg/style 
               {:stroke (str "rgba(163,190,140," (/ (inc a) 10.0) ")")
                :stroke-width "2px"
                :fill "none"}))))
    (svg/translate [100 100]))))

;; use hiccup or your favourite hiccup compiler.
;; the SVG library works in reagent as well.
(html circles)
```

### Deps.edn

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
| no layout engine                             | layout engine                                   |
| no built-in rasterization                    | rasterize SVGs using Batik                      |
| Clojure and Clojurescript                    | Clojure only                                    |
| SVG primitives only (for now)                | SVG primitives + 'prefabs' (eg. markers/arrows) |

Stay tuned for updates!
