(ns examples.offset
  (:require [hiccup.core :refer [html]]
            [svg-clj.composites :as comp :refer [svg]]
            [svg-clj.elements :as el]
            [svg-clj.parametric :as p]
            [svg-clj.transforms :as tf]
            [svg-clj.utils :as u]))

(def a (-> (p/regular-polygon-pts 120 10)
           (el/polygon)
           (tf/style {:fill "none"
                      :stroke "red"
                      :stroke-width "3px"})))

(def b (-> (tf/offset a 20)
           (tf/style {:fill "none"
                      :stroke "limegreen"
                      :stroke-width "3px"})))

(def c (-> (el/circle 40)
           (tf/style {:fill "none"
                      :stroke "red"
                      :stroke-width "3px"})))

(def d (-> (tf/offset c -3)
           (tf/style {:fill "none"
                      :stroke "limegreen"
                      :stroke-width "3px"})))

(def pts [ [0 0] [100 -300] [200 -300] [300 0]])
(def curve (p/bezier pts))

(def e (-> (map curve (range 0 1.05 0.05))
           (el/polyline)
           (tf/style {:fill "none"
                      :stroke "red"
                      :stroke-width "3px"})))

(def f (-> (map curve (range 0 1.05 0.05))
           (u/offset-pts 20)
           (el/polyline)
           (tf/style {:fill "none"
                      :stroke "limegreen"
                      :stroke-width "3px"})))

(def examples [a
               b
               c
               d
               (el/g a b)
               (el/g c d)
               e
               f
               (el/g e f)])

(def doc
  (->>
   (for [elem examples]
     (-> elem
         svg
         (tf/style {:style {:outline "1px solid blue"
                            :margin "10px"}})))
   (partition-all 3)
   (interpose [:br])))

(spit
 "examples/offset.html"
 (html
  [:html
   [:body
    [:h1 "Offset Examples"]
    doc]]))
