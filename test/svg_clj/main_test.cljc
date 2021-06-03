(ns svg-clj.main-test
  (:require 
   [svg-clj.elements :as svg
    :refer [circle
            ellipse
            g
            image
            line
            polygon
            polyline
            rect
            text]]
   [svg-clj.composites :as cp
    :refer [svg]]
   [svg-clj.transforms :as tf
    :refer [translate
            rotate
            centroid
            bounds
            scale
            style]]
   [svg-clj.path :as path]
   [clojure.test
    :refer [deftest is]]))

;; just starting with some basics. 
;; more complete tests coming soon

(def test-circle (circle 5))
(def test-ellipse (ellipse 5 10))
(def test-line (line [0 0] [10 20]))
(def test-path (path/polygon [ [0 0] [10 20] [40 50] [20 10] ]))
(def test-polygon (polygon [ [0 0] [10 20] [40 50] [20 10] ]))
(def test-polyline (polyline [ [0 0] [10 20] [40 50] [20 10] ]))
(def test-rect (rect 60 30))
(def test-image (image "https://www.fillmurray.com/g/200/300" 200 300))
(def test-g (g test-circle
               test-ellipse
               test-line
               test-path
               test-polygon
               test-polyline
               test-rect
               test-image))

(def test-shapes [test-circle
                  test-ellipse
                  test-line
                  test-path
                  test-polygon
                  test-polyline
                  test-rect
                  test-image])

(deftest basic-shapes-test
  (is (= test-circle [:circle {:cx 0 :cy 0 :r 5}]))
  (is (= test-ellipse [:ellipse {:cx 0 :cy 0 :rx 5 :ry 10}]))
  (is (= test-line [:line {:x1 0 :y1 0 :x2 10 :y2 20}]))
  (is (= test-path [:path {:d "M0 0 L10 20 L40 50 L20 10 Z"
                           :fill-rule "evenodd"}]))
  (is (= test-polygon [:polygon {:points "0,0 10,20 40,50 20,10"}]))
  (is (= test-polyline [:polyline {:points "0,0 10,20 40,50 20,10"}]))
  (is (= test-rect [:rect {:x -30.0 :y -15.0 :width 60 :height 30}]))
  (is (= test-image [:image 
                     {:href "https://www.fillmurray.com/g/200/300"
                      :x -100.0 :y -150.0 
                      :width 200 :height 300}])))

(deftest basic-translate-test
  (is (= (-> test-circle (translate [10 10]))
         [:circle {:r 5 :cx 10 :cy 10 :transform "rotate(0 10 10)"}]))
  (is (= (-> test-ellipse (translate [10 10]))
         [:ellipse {:rx 5 :ry 10 :cx 10 :cy 10 :transform "rotate(0 10 10)"}]))
  (is (= (-> test-line (translate [10 10]))
         [:line {:x1 10 :y1 10 :x2 20 :y2 30}]))
  (is (= (-> test-path (translate [10 10]))
         [:path {:d "M10 10 L20 30 L50 60 L30 20 Z"
                 :fill-rule "evenodd"}]))
  (is (= (-> test-polygon (translate [10 10]))
         [:polygon {:points "10,10 20,30 50,60 30,20"}]))
  (is (= (-> test-polyline (translate [10 10]))
         [:polyline {:points "10,10 20,30 50,60 30,20"}]))
  (is (= (-> test-rect (translate [10 10]))
         [:rect {:x -20.0 :y -5.0 :width 60 :height 30 :transform "rotate(0 10.0 10.0)"}]))
  (is (= (-> test-image (translate [10 10]))
         [:image {:href "https://www.fillmurray.com/g/200/300"
                  :x -90.0 :y -140.0
                  :width 200 :height 300
                  :transform "rotate(0 10.0 10.0)"}])))

(deftest translate-group-test
  (is (= (drop 2 (translate test-g [5 10]))
         (map #(translate % [5 10]) (drop 2 test-g)))))

(deftest translate-list-test
  (let [a (repeat 10 (rect 10 20))]
    (is (= (translate a [5 10])
           (map #(translate % [5 10]) a)))))

(deftest basic-rotate-test
  (is (= (-> test-circle (rotate 45))
         [:circle {:r 5 :cx 0 :cy 0 :transform "rotate(45 0 0)"}]))
  (is (= (-> test-ellipse (rotate 45))
         [:ellipse {:rx 5 :ry 10 :cx 0 :cy 0 :transform "rotate(45 0 0)"}]))
  (is (= (-> test-line (rotate 90))
         [:line {:x1 15.0 :y1 4.999999999999999 :x2 -5.0 :y2 15.0}]))
  (is (= (-> test-path (rotate 90))
         [:path {:d "M37.5 2.5 L17.5 12.5 L-12.5 42.5 L27.5 22.5 Z"
                 :fill-rule "evenodd"}]))
  (is (= (-> test-polygon (rotate 90))
         [:polygon {:points "37.5,2.5 17.5,12.5 -12.5,42.5 27.5,22.5"}]))
  (is (= (-> test-polyline (rotate 90))
         [:polyline {:points "37.5,2.5 17.5,12.5 -12.5,42.5 27.5,22.5"}]))
  (is (= (-> test-rect (rotate 45))
         [:rect {:x -30.0 :y -15.0 :width 60 :height 30 :transform "rotate(45 0.0 0.0)"}]))
  (is (= (-> test-image (rotate 45))
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
   [:path
    {:d "M0.0 0.0 L-20.0 10.0 L-50.0 40.0 L-10.0 20.0 Z" :fill-rule "evenodd"}]
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
  (is (not= (drop 2 (rotate test-g 45))
            (map #(rotate % 45) (drop 2 test-g))))
  (is (= (rotate test-g 90)
         rotated-test-g-data-structure)))

(deftest rotate-list-test
  (let [a (repeat 10 (rect 10 20))]
    (is (= (rotate a 45)
           (map #(rotate % 45) a)))))
