(ns examples.basics
  (:require [hiccup.core :refer [html]]
            [svg-clj.path :refer [circle-path
                                  line-path
                                  rect-path
                                  polyline-path
                                  polygon-path
                                  bezier
                                  arc]]
            [svg-clj.main :refer [svg
                                  circle
                                  ellipse
                                  rect
                                  line
                                  polygon
                                  polyline
                                  text
                                  g
                                  image
                                  style
                                  centroid
                                  bounds
                                  translate
                                  rotate
                                  rotate-pt
                                  scale
                                  merge-paths]]))

(defn show-debug-geom
  [elem]
  (let [ctr (centroid elem)
        bds (bounds elem)]
    (g elem
       (g (->> (polygon bds)
               (style {:fill "none"
                       :stroke "red"
                       :stroke-width "3px"}))
          (->> (circle 2)
               (translate ctr)
               (style {:fill "red"}))))))

(def a (g (->> (circle 50)
               (translate [100 100])
               (style {:fill "pink"
                       :stroke-width "5px"
                       :stroke "hotpink"}))
          (->> (circle 10)
               (translate [15 15])
               (style {:fill "pink"
                       :stroke-width "5px"
                       :stroke "hotpink"}))))

(def basic-group
  (g
   (rect 20 20)
   (->> (rect 20 20) (translate [20 0]))
   (->> (rect 20 20) (translate [0 20]))
   (->> (rect 20 20) (translate [20 20]))))

(def circles
  (svg 
   [200 200 1]
   (->>
    (apply g (for [a (range 0 12)]
         (->> (circle (+ 5 (* a 4)))
              (translate [(/ (+ 5 (* a 4)) 2) 0])
              (translate (rotate-pt (* a -40) [20 0]))
              (style {:stroke 
                      (str "rgba(163,190,140," 
                           (/ (inc a) 10.0) ")")
                      :stroke-width "2px"
                      :fill "none"}))))
    (translate [100 100]))))

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
     (->> 
      (svg [200 200 1]
           (->> elem
                (translate [100 100])
                (rotate 90)
                (style {:fill "pink"
                        :stroke-width "2px"
                        :stroke "hotpink"})
                show-debug-geom))
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
    #_circles]]))
