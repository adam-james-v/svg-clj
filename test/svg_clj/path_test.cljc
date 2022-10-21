(ns svg-clj.path-test
  (:require
   [svg-clj.elements :as el]
   [svg-clj.path :as path]
   [svg-clj.transforms :as tf]
   [clojure.test :as test :refer [deftest is]]))

(def test-circle (path/circle 5))
(def test-ellipse (path/ellipse 5 10))
(def test-line (path/line [0 0] [10 20]))
(def test-polygon (path/polygon [ [0 0] [10 20] [40 50] [20 10] ]))
(def test-polyline (path/polyline [ [0 0] [10 20] [40 50] [20 10] ]))
(def test-rect (path/rect 60 30))
(def test-g (el/g test-circle
                  test-ellipse
                  test-line
                  test-polygon
                  test-polyline
                  test-rect))

(def test-shapes [test-circle
                  test-ellipse
                  test-line
                  test-polygon
                  test-polyline
                  test-rect])

(deftest basic-shapes-test
  (is (= test-circle [:path {:d "M5 0 A5 5 0 1 0 0 5 A5 5 0 1 0 -5 0 A5 5 0 1 0 0 -5 A5 5 0 1 0 5 0 Z", :fill-rule "evenodd"}]))
  (is (= test-ellipse [:path {:d "M5 0 A5 10 0 1 0 0 10 A5 10 0 1 0 -5 0 A5 10 0 1 0 0 -10 A5 10 0 1 0 5 0 Z", :fill-rule "evenodd"}]))
  (is (= test-line [:path {:d "M0 0 L10 20", :fill-rule "evenodd"}]))
  (is (= test-polygon [:path {:d "M0 0 L10 20 L40 50 L20 10 Z", :fill-rule "evenodd"}]))
  (is (= test-polyline [:path {:d "M0 0 L10 20 L40 50 L20 10", :fill-rule "evenodd"}]))
  (is (= test-rect [:path {:d "M-30.0 -15.0 L30.0 -15.0 L30.0 15.0 L-30.0 15.0 Z", :fill-rule "evenodd"}])))

(def a-str "M453 83l15 -9q-13 -81 -96 -81q-34 0 -51 22.5t-20 53.5q-20 -23 -30.5 -33t-39.5 -27.5t-58 -17.5q-50 0 -86 28t-36 89q0 27 11.5 47t32 32.5t41.5 20.5t49 11.5t46.5 4.5t41 1.5t24.5 0.5q3 0 3 74q0 95 -87 95q-30 0 -48 -13t-22 -31q29 0 29 -44q0 -27 -18 -38.5\nt-35 -11.5q-19 0 -34 17.5t-15 38.5q0 37 47 72q38 29 105 29q63 0 112 -39t49 -106q0 -33 -2 -92t-2 -85q0 -67 31 -66q18 0 27 13t16 44zM297 94v118q-35 -3 -64.5 -6.5t-50.5 -11t-33 -25.5t-12 -50q0 -43 21.5 -63t54.5 -20q52 0 84 58z")

(def a-cmds (path/path-str->cmds a-str))

(deftest basic-cmds-test
  (is (= #{:abs} (set (map :coordsys a-cmds))))
  (is (nil? (#{"V" "H" "S" "T"} (set (map :command a-cmds))))))

(deftest basic-translate-test
  (is (= (-> test-circle (tf/translate [10 10]))
         [:path {:d "M15 10 A5 5 0 1 0 10 15 A5 5 0 1 0 5 10 A5 5 0 1 0 10 5 A5 5 0 1 0 15 10 Z", :fill-rule "evenodd"}]))
  (is (= (-> test-ellipse (tf/translate [10 10]))
         [:path {:d "M15 10 A5 10 0 1 0 10 20 A5 10 0 1 0 5 10 A5 10 0 1 0 10 0 A5 10 0 1 0 15 10 Z", :fill-rule "evenodd"}]))
  (is (= (-> test-line (tf/translate [10 10]))
         [:path {:d "M10 10 L20 30", :fill-rule "evenodd"}]))
  (is (= (-> test-polygon (tf/translate [10 10]))
         [:path {:d "M10 10 L20 30 L50 60 L30 20 Z", :fill-rule "evenodd"}]))
  (is (= (-> test-polyline (tf/translate [10 10]))
         [:path {:d "M10 10 L20 30 L50 60 L30 20", :fill-rule "evenodd"}]))
  (is (= (-> test-rect (tf/translate [10 10]))
         [:path {:d "M-20.0 -5.0 L40.0 -5.0 L40.0 25.0 L-20.0 25.0 Z", :fill-rule "evenodd"}])))

(deftest translate-group-test
  (is (= (drop 2 (tf/translate test-g [5 10]))
         (map #(tf/translate % [5 10]) (drop 2 test-g)))))

(deftest translate-list-test
  (let [a (repeat 10 (el/rect 10 20))]
    (is (= (tf/translate a [5 10])
           (map #(tf/translate % [5 10]) a)))))

(deftest basic-rotate-test
  (is (= (-> test-circle (tf/rotate 45))
         [:path {:d "M3.5355339059327378 3.5355339059327373 A5 5 45 1 0 -3.5355339059327373 3.5355339059327378 A5 5 45 1 0 -3.5355339059327378 -3.5355339059327373 A5 5 45 1 0 3.5355339059327373 -3.5355339059327378 A5 5 45 1 0 3.5355339059327378 3.5355339059327373 Z" :fill-rule "evenodd"}]))
  (is (= (-> test-ellipse (tf/rotate 45))
         [:path {:d "M3.5355339059327378 3.5355339059327373 A5 10 45 1 0 -7.071067811865475 7.0710678118654755 A5 10 45 1 0 -3.5355339059327378 -3.5355339059327373 A5 10 45 1 0 7.071067811865475 -7.0710678118654755 A5 10 45 1 0 3.5355339059327378 3.5355339059327373 Z" :fill-rule "evenodd"}]))
  (is (= (-> test-line (tf/rotate 90))
         [:path {:d "M15.0 4.999999999999999 L-5.0 15.0", :fill-rule "evenodd"}]))
  (is (= (-> test-polygon (tf/rotate 90))
         [:path {:d "M37.5 2.5 L17.5 12.5 L-12.5 42.5 L27.5 22.5 Z", :fill-rule "evenodd"}]))
  (is (= (-> test-polyline (tf/rotate 90))
         [:path {:d "M37.5 2.5 L17.5 12.5 L-12.5 42.5 L27.5 22.5", :fill-rule "evenodd"}]))
  (is (= (-> test-rect (tf/rotate 45))
         [:path {:d "M-10.606601717798215 -31.819805153394636 L31.81980515339464 10.60660171779821 L10.606601717798215 31.819805153394636 L-31.81980515339464 -10.60660171779821 Z",
  :fill-rule "evenodd"}])))

(def rotated-test-g-data-structure
  [:g {}
   [:path {:d "M22.5 17.5 A5 5 90 1 0 17.5 12.5 A5 5 90 1 0 22.5 7.5 A5 5 90 1 0 27.5 12.5 A5 5 90 1 0 22.5 17.5 Z", :fill-rule "evenodd"}]
   [:path {:d "M22.5 17.5 A5 10 90 1 0 12.5 12.5 A5 10 90 1 0 22.5 7.5 A5 10 90 1 0 32.5 12.5 A5 10 90 1 0 22.5 17.5 Z", :fill-rule "evenodd"}]
   [:path {:d "M22.5 12.5 L2.5 22.5", :fill-rule "evenodd"}]
   [:path {:d "M22.5 12.5 L2.5000000000000004 22.5 L-27.5 52.5 L12.5 32.5 Z", :fill-rule "evenodd"}]
   [:path {:d "M22.5 12.5 L2.5000000000000004 22.5 L-27.5 52.5 L12.5 32.5", :fill-rule "evenodd"}]
   [:path {:d "M37.5 -17.5 L37.5 42.5 L7.500000000000002 42.5 L7.499999999999998 -17.5 Z", :fill-rule "evenodd"}]])

(deftest rotate-group-test
  (is (not= (drop 2 (tf/rotate test-g 45))
            (map #(tf/rotate % 45) (drop 2 test-g))))
  (is (= (tf/rotate test-g 90)
         rotated-test-g-data-structure)))

(deftest rotate-list-test
  (let [a (repeat 10 (el/rect 10 20))]
    (is (= (tf/rotate a 45)
           (map #(tf/rotate % 45) a)))))
