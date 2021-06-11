(ns examples.basics
  (:require [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [hiccup.core :refer [html]]
            [svg-clj.utils :as utils]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.elements :as svg]
            [svg-clj.composites :refer [svg]]
            [svg-clj.tools :refer [cider-show]]))

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

(def a (svg/g (-> (svg/circle 50)
              (tf/translate [100 100])
              (tf/style {:fill "pink"
                      :stroke-width "5px"
                      :stroke "hotpink"}))
          (-> (svg/circle 10)
              (tf/translate [15 15])
              (tf/style {:fill "pink"
                      :stroke-width "5px"
                      :stroke "hotpink"}))))

(def basic-group
  (svg/g
   (svg/rect 20 20)
   (-> (svg/rect 20 20) (tf/translate [20 0]))
   (-> (svg/rect 20 20) (tf/translate [0 20]))
   (-> (svg/rect 20 20) (tf/translate [20 20]))))

(def circles
  (-> (svg/g (for [a (range 0 12)]
           (-> (svg/circle (+ 5 (* a 4)))
               (tf/translate [(/ (+ 5 (* a 4)) 2) 0])
               (tf/translate (utils/rotate-pt [20 0] (* a -40)))
               (tf/style {:stroke 
                       (str "rgba(163,190,140," 
                            (/ (inc a) 10.0) ")")
                       :stroke-width "2px"
                       :fill "none"}))))
      (tf/translate [100 100])
      (svg 200 200)))

(def basics [(arc [0 0] [50 0] 90)
             (circle-path 40)
             (bezier [0 0] [30 20] [80 40] [120 180])
             (circle 80)
             (rect-path 70 120)
             (ellipse 40 80)
             (line [0 0] [100 100])
             (line-path [0 0] [100 100])
             (polygon [ [0 0] [30 0] [30 20] [15 10] [0 20] ])
             (polyline [ [0 0] [30 0] [30 20] [15 10] [0 20] ])
             (polygon-path [ [0 0] [30 0] [30 20] [15 10] [0 20] ])
             (text "this is text")
             (image "https://www.fillmurray.com/300/200" 100 67)
             (merge-paths (rect-path 100 100) (rect-path 80 80))
             basic-group])

(def doc
  (->>
   (for [elem basics]
     (-> elem
         (translate [80 80])
         (rotate 20)
         (style {:fill "pink"
                 :stroke-width "2px"
                 :stroke "hotpink"})
         show-debug-geom
         (svg 200 200)
         (style {:style {:outline "1px solid blue"
                         :margin "10px"}})))
   (partition-all 3)
   (interpose [:br])))

(spit 
 "examples/basics.html"
 (html 
  [:html 
   [:body
    [:h1 "Basic Geometry Examples"]
    doc
    circles]]))
