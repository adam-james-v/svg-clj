(ns svg-clj.utils-test
  (:require [svg-clj.utils :as utils]
            [svg-clj.parametric :as p]
            [svg-clj.transforms :as tf]
            [clojure.test :as test :refer [deftest is]]))

(deftest angle-from-pts
  (let [angles (map
                #(utils/angle-from-pts [10 0] [0 0] %)
                (p/regular-polygon-pts 10 20))
        sorted-angles (reverse (sort angles))]
    (is (= (rest (map #(Math/round %) angles)) ;; first angle is 0, rest are in decreasing order
           (drop-last (map #(Math/round %) sorted-angles))))))

(deftest angle-first-quadrant
  (let [eps 0.00001
        a (utils/angle-from-pts [0 10] [0 0] [10 0])]
    (is (< (Math/abs (- 90.0 a)) eps))))

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
