(ns examples.basics
  "Showing the basic elements and transforms of svg-clj."
  (:require [hiccup.core :refer [html]]
            [svg-clj.composites :refer [svg]]
            [svg-clj.elements :as el]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.utils :as u]))

(defn show-debug-geom
  "Add some useful debugging geometry to `elem`."
  [elem]
  (let [ctr (tf/centroid elem)
        bds (tf/bounds elem)]
    (el/g elem
       (el/g (-> (el/polygon bds)
                 (tf/style {:fill "none"
                            :stroke "red"
                            :stroke-width "1px"
                            :opacity 0.3}))
          (-> (el/circle 2)
               (tf/translate ctr)
               (tf/style {:fill "red"
                          :opacity 0.3}))))))

(def basic-group
  (el/g
   (el/rect 20 20)
   (-> (el/rect 20 20) (tf/translate [20 0]))
   (-> (el/rect 20 20) (tf/translate [0 20]))
   (-> (el/rect 20 20) (tf/translate [20 20]))))

(def circles
  (-> (for [a (range 0 12)]
        (-> (el/circle (+ 5 (* a 4)))
            (tf/translate [(/ (+ 5 (* a 4)) 2) 0])
            (tf/translate (u/rotate-pt [20 0] (* a -40)))
            (tf/style {:stroke
                       (str "rgba(163,190,140,"
                            (/ (inc a) 10.0) ")")
                       :stroke-width "2px"
                       :fill "none"})))
      el/g
      (tf/translate [100 100])))

(def basics [(path/arc [0 0] [50 0] 90)
             (path/circle 40)
             (path/bezier [0 0] [30 20] [80 40] [120 180])
             (el/circle 80)
             (path/rect 70 120)
             (path/ellipse 40 80)
             (el/line [0 0] [100 100])
             (path/line [0 0] [100 100])
             (el/polygon [ [0 0] [30 0] [30 20] [15 10] [0 20] ])
             (el/polyline [ [0 0] [30 0] [30 20] [15 10] [0 20] ])
             (path/polygon [ [0 0] [30 0] [30 20] [15 10] [0 20] ])
             (el/text "this is text")
             (el/image "https://www.fillmurray.com/300/200" 100 67)
             (path/merge-paths (path/rect 100 100) (path/rect 80 80))
             basic-group
             circles])

(def doc
  (->>
   (for [elem basics]
     (-> elem
         (tf/translate [80 80])
         (tf/rotate 20)
         (tf/style {:fill "pink"
                 :stroke-width "2px"
                 :stroke "hotpink"})
         show-debug-geom
         svg
         (tf/style {:style {:outline "1px solid blue"
                         :margin "10px"}})))
   (partition-all 3)
   (interpose [:br])))

(spit
 "examples/basics.html"
 (html
  [:html
   [:body
    [:h1 "Basic Geometry Examples"]
    doc]]))
