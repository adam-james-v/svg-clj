(def test-circle (el/circle 5))
(def test-ellipse (el/ellipse 5 10))
(def test-line (el/line [0 0] [10 20]))
(def test-polygon (el/polygon [ [0 0] [10 20] [40 50] [20 10] ]))
(def test-polyline (el/polyline [ [0 0] [10 20] [40 50] [20 10] ]))
(def test-rect (el/rect 60 30))
(def test-image (el/image "https://www.fillmurray.com/g/200/300" 200 300))
(def test-g (el/g test-circle
                  test-ellipse
                  test-line
                  test-polygon
                  test-polyline
                  test-rect
                  test-image))

(def test-shapes [test-circle
                  test-ellipse
                  test-line
                  test-polygon
                  test-polyline
                  test-rect
                  test-image])

(deftest basic-shapes-test
  (is (= test-circle [:circle {:cx 0 :cy 0 :r 5}]))
  (is (= test-ellipse [:ellipse {:cx 0 :cy 0 :rx 5 :ry 10}]))
  (is (= test-line [:line {:x1 0 :y1 0 :x2 10 :y2 20}]))
  (is (= test-polygon [:polygon {:points "0,0 10,20 40,50 20,10"}]))
  (is (= test-polyline [:polyline {:points "0,0 10,20 40,50 20,10"}]))
  (is (= test-rect [:rect {:x -30.0 :y -15.0 :width 60 :height 30}]))
  (is (= test-image [:image 
                     {:href "https://www.fillmurray.com/g/200/300"
                      :x -100.0 :y -150.0 
                      :width 200 :height 300}])))

(deftest basic-translate-test
  (is (= (-> test-circle (tf/translate [10 10]))
         [:circle {:r 5 :cx 10 :cy 10 :transform "rotate(0 10 10)"}]))
  (is (= (-> test-ellipse (tf/translate [10 10]))
         [:ellipse {:rx 5 :ry 10 :cx 10 :cy 10 :transform "rotate(0 10 10)"}]))
  (is (= (-> test-line (tf/translate [10 10]))
         [:line {:x1 10 :y1 10 :x2 20 :y2 30}]))
  (is (= (-> test-polygon (tf/translate [10 10]))
         [:polygon {:points "10,10 20,30 50,60 30,20"}]))
  (is (= (-> test-polyline (tf/translate [10 10]))
         [:polyline {:points "10,10 20,30 50,60 30,20"}]))
  (is (= (-> test-rect (tf/translate [10 10]))
         [:rect {:x -20.0 :y -5.0 :width 60 :height 30 :transform "rotate(0 10.0 10.0)"}]))
  (is (= (-> test-image (tf/translate [10 10]))
         [:image {:href "https://www.fillmurray.com/g/200/300"
                  :x -90.0 :y -140.0
                  :width 200 :height 300
                  :transform "rotate(0 10.0 10.0)"}])))

(deftest translate-group-test
  (is (= (drop 2 (tf/translate test-g [5 10]))
         (map #(tf/translate % [5 10]) (drop 2 test-g)))))

(deftest translate-list-test
  (let [a (repeat 10 (el/rect 10 20))]
    (is (= (tf/translate a [5 10])
           (map #(tf/translate % [5 10]) a)))))

(deftest basic-rotate-test
  (is (= (-> test-circle (tf/rotate 45))
         [:circle {:r 5 :cx 0 :cy 0 :transform "rotate(45 0 0)"}]))
  (is (= (-> test-ellipse (tf/rotate 45))
         [:ellipse {:rx 5 :ry 10 :cx 0 :cy 0 :transform "rotate(45 0 0)"}]))
  (is (= (-> test-line (tf/rotate 90))
         [:line {:x1 15.0 :y1 4.999999999999999 :x2 -5.0 :y2 15.0}]))
  (is (= (-> test-polygon (tf/rotate 90))
         [:polygon {:points "37.5,2.5 17.5,12.5 -12.5,42.5 27.5,22.5"}]))
  (is (= (-> test-polyline (tf/rotate 90))
         [:polyline {:points "37.5,2.5 17.5,12.5 -12.5,42.5 27.5,22.5"}]))
  (is (= (-> test-rect (tf/rotate 45))
         [:rect {:x -30.0 :y -15.0 :width 60 :height 30 :transform "rotate(45 0.0 0.0)"}]))
  (is (= (-> test-image (tf/rotate 45))
         [:image {:href "https://www.fillmurray.com/g/200/300"
                  :x -100.0 :y -150.0
                  :width 200 :height 300
                  :transform "rotate(45 0.0 0.0)"}])))

(def rotated-test-g-data-structure
  [:g
   {}
   [:circle {:cx 0.0 :cy 0.0 :r 5 :transform "rotate(90 0.0 0.0)"}]
   [:ellipse {:cx 0.0 :cy 0.0 :rx 5 :ry 10 :transform "rotate(90 0.0 0.0)"}]
   [:line {:x1 0.0 :y1 0.0 :x2 -20.0 :y2 10.000000000000002}]
   [:polygon {:points "0.0,0.0 -20.0,10.0 -50.0,40.0 -10.0,20.0"}]
   [:polyline {:points "0.0,0.0 -20.0,10.0 -50.0,40.0 -10.0,20.0"}]
   [:rect
    {:width 60 :height 30 :x -30.0 :y -15.0 :transform "rotate(90 0.0 0.0)"}]
   [:image
    {:href "https://www.fillmurray.com/g/200/300"
     :width 200
     :height 300
     :x -100.0
     :y -150.0
     :transform "rotate(90 0.0 0.0)"}]])

(deftest rotate-group-test
  (is (not= (drop 2 (tf/rotate test-g 45))
            (map #(tf/rotate % 45) (drop 2 test-g))))
  (is (= (tf/rotate test-g 90)
         rotated-test-g-data-structure)))

(deftest rotate-list-test
  (let [a (repeat 10 (el/rect 10 20))]
    (is (= (tf/rotate a 45)
           (map #(tf/rotate % 45) a)))))

(deftest cast-numerical-attrs-test
  (let [attrs {:cx "10" :cy "20" :width "200" :height "200px"}
        {:keys [cx cy width height] :as res} (utils/cast-numerical-attrs attrs)]
    (is (= cx 10))
    (is (= cy 20))
    (is (= width 200))
    (is (= height "200px"))))

(deftest basic-string-to-elements
  (let [s "<rect width=\"10\" height=\"40\" x=\"50\" y=\"60\" />"
        res (utils/svg-str->elems s)
        [k props] (first res)]
    (is (= 1 (count res)))
    (is (= k :rect))
    (is (= (set (keys props)) #{:width :height :x :y}))))

(ns svg-clj.elements-test
  (:require [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.transforms :as tf]
            [clojure.test :refer [deftest is]]))

(def test-circle (el/circle 5))
(def test-ellipse (el/ellipse 5 10))
(def test-line (el/line [0 0] [10 20]))
(def test-polygon (el/polygon [ [0 0] [10 20] [40 50] [20 10] ]))
(def test-polyline (el/polyline [ [0 0] [10 20] [40 50] [20 10] ]))
(def test-rect (el/rect 60 30))
(def test-image (el/image "https://www.fillmurray.com/g/200/300" 200 300))
(def test-g (el/g test-circle
                  test-ellipse
                  test-line
                  test-polygon
                  test-polyline
                  test-rect
                  test-image))

(def test-shapes [test-circle
                  test-ellipse
                  test-line
                  test-polygon
                  test-polyline
                  test-rect
                  test-image])

(deftest basic-shapes-test
  (is (= test-circle [:circle {:cx 0 :cy 0 :r 5}]))
  (is (= test-ellipse [:ellipse {:cx 0 :cy 0 :rx 5 :ry 10}]))
  (is (= test-line [:line {:x1 0 :y1 0 :x2 10 :y2 20}]))
  (is (= test-polygon [:polygon {:points "0,0 10,20 40,50 20,10"}]))
  (is (= test-polyline [:polyline {:points "0,0 10,20 40,50 20,10"}]))
  (is (= test-rect [:rect {:x -30.0 :y -15.0 :width 60 :height 30}]))
  (is (= test-image [:image 
                     {:href "https://www.fillmurray.com/g/200/300"
                      :x -100.0 :y -150.0 
                      :width 200 :height 300}])))

(deftest basic-translate-test
  (is (= (-> test-circle (tf/translate [10 10]))
         [:circle {:r 5 :cx 10 :cy 10 :transform "rotate(0 10 10)"}]))
  (is (= (-> test-ellipse (tf/translate [10 10]))
         [:ellipse {:rx 5 :ry 10 :cx 10 :cy 10 :transform "rotate(0 10 10)"}]))
  (is (= (-> test-line (tf/translate [10 10]))
         [:line {:x1 10 :y1 10 :x2 20 :y2 30}]))
  (is (= (-> test-polygon (tf/translate [10 10]))
         [:polygon {:points "10,10 20,30 50,60 30,20"}]))
  (is (= (-> test-polyline (tf/translate [10 10]))
         [:polyline {:points "10,10 20,30 50,60 30,20"}]))
  (is (= (-> test-rect (tf/translate [10 10]))
         [:rect {:x -20.0 :y -5.0 :width 60 :height 30 :transform "rotate(0 10.0 10.0)"}]))
  (is (= (-> test-image (tf/translate [10 10]))
         [:image {:href "https://www.fillmurray.com/g/200/300"
                  :x -90.0 :y -140.0
                  :width 200 :height 300
                  :transform "rotate(0 10.0 10.0)"}])))

(deftest translate-group-test
  (is (= (drop 2 (tf/translate test-g [5 10]))
         (map #(tf/translate % [5 10]) (drop 2 test-g)))))

(deftest translate-list-test
  (let [a (repeat 10 (el/rect 10 20))]
    (is (= (tf/translate a [5 10])
           (map #(tf/translate % [5 10]) a)))))

(deftest basic-rotate-test
  (is (= (-> test-circle (tf/rotate 45))
         [:circle {:r 5 :cx 0 :cy 0 :transform "rotate(45 0 0)"}]))
  (is (= (-> test-ellipse (tf/rotate 45))
         [:ellipse {:rx 5 :ry 10 :cx 0 :cy 0 :transform "rotate(45 0 0)"}]))
  (is (= (-> test-line (tf/rotate 90))
         [:line {:x1 15.0 :y1 4.999999999999999 :x2 -5.0 :y2 15.0}]))
  (is (= (-> test-polygon (tf/rotate 90))
         [:polygon {:points "37.5,2.5 17.5,12.5 -12.5,42.5 27.5,22.5"}]))
  (is (= (-> test-polyline (tf/rotate 90))
         [:polyline {:points "37.5,2.5 17.5,12.5 -12.5,42.5 27.5,22.5"}]))
  (is (= (-> test-rect (tf/rotate 45))
         [:rect {:x -30.0 :y -15.0 :width 60 :height 30 :transform "rotate(45 0.0 0.0)"}]))
  (is (= (-> test-image (tf/rotate 45))
         [:image {:href "https://www.fillmurray.com/g/200/300"
                  :x -100.0 :y -150.0
                  :width 200 :height 300
                  :transform "rotate(45 0.0 0.0)"}])))

(def rotated-test-g-data-structure
  [:g
   {}
   [:circle {:cx 0.0 :cy 0.0 :r 5 :transform "rotate(90 0.0 0.0)"}]
   [:ellipse {:cx 0.0 :cy 0.0 :rx 5 :ry 10 :transform "rotate(90 0.0 0.0)"}]
   [:line {:x1 0.0 :y1 0.0 :x2 -20.0 :y2 10.000000000000002}]
   [:polygon {:points "0.0,0.0 -20.0,10.0 -50.0,40.0 -10.0,20.0"}]
   [:polyline {:points "0.0,0.0 -20.0,10.0 -50.0,40.0 -10.0,20.0"}]
   [:rect
    {:width 60 :height 30 :x -30.0 :y -15.0 :transform "rotate(90 0.0 0.0)"}]
   [:image
    {:href "https://www.fillmurray.com/g/200/300"
     :width 200
     :height 300
     :x -100.0
     :y -150.0
     :transform "rotate(90 0.0 0.0)"}]])

(deftest rotate-group-test
  (is (not= (drop 2 (tf/rotate test-g 45))
            (map #(tf/rotate % 45) (drop 2 test-g))))
  (is (= (tf/rotate test-g 90)
         rotated-test-g-data-structure)))

(deftest rotate-list-test
  (let [a (repeat 10 (el/rect 10 20))]
    (is (= (tf/rotate a 45)
           (map #(tf/rotate % 45) a)))))
