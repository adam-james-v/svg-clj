(ns svg-clj.main-test
  (:require [svg-clj.main :as svg]
            [clojure.test :refer [deftest is]]))

;; just starting with some basics. 
;; more complete tests coming soon

(def test-circle (circle 5))
(def test-ellipse (ellipse 5 10))
(def test-line (line [0 0] [10 20]))
(def test-path (polygon-path [ [0 0] [10 20] [40 50] [20 10] ]))
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
                  #_test-ellipse
                  #_test-line
                  #_test-path
                  #_test-polygon
                  #_test-polyline
                  #_test-rect
                  #_test-image])

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
  (is (= (->> test-circle (translate [10 10]))
         [:circle {:r 5 :cx 10 :cy 10 :transform "rotate(0 10 10)"}]))
  (is (= (->> test-ellipse (translate [10 10]))
         [:ellipse {:rx 5 :ry 10 :cx 10 :cy 10 :transform "rotate(0 10 10)"}]))
  (is (= (->> test-line (translate [10 10]))
         [:line {:x1 10 :y1 10 :x2 20 :y2 30}]))
  (is (= (->> test-path (translate [10 10]))
         [:path {:d "M10 10 L20 30 L50 60 L30 20 Z"
                 :fill-rule "evenodd"}]))
  (is (= (->> test-polygon (translate [10 10]))
         [:polygon {:points "10,10 20,30 50,60 30,20"}]))
  (is (= (->> test-polyline (translate [10 10]))
         [:polyline {:points "10,10 20,30 50,60 30,20"}]))
  (is (= (->> test-rect (translate [10 10]))
         [:rect {:x -20.0 :y -5.0 :width 60 :height 30 :transform "rotate(0 10.0 10.0)"}]))
  (is (= (->> test-image (translate [10 10]))
         [:image {:href "https://www.fillmurray.com/g/200/300"
                  :x -90.0 :y -140.0
                  :width 200 :height 300
                  :transform "rotate(0 10.0 10.0)"}])))
