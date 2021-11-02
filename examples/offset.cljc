(ns examples.offset
  (:require [clojure.string :as str]
            [hiccup.core :refer [html]]
            [svg-clj.composites :as cp :refer [svg]]
            [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.parametric :as p]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.layout :as lo]
            #?(:clj [svg-clj.tools :as tools])))

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
           (tf/offset-pts 20)
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

(ns examples.algorithms
  (:require [clojure.string :as str]
            [hiccup.core :refer [html]]
            [svg-clj.composites :as cp :refer [svg]]
            [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.parametric :as p]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.layout :as lo]
            [svg-clj.algorithms :as alg]
            #?(:clj [svg-clj.tools :as tools])))

(defn inside-pt-example
  []
  (let [deg (rand-int 361)
        tri (->> (p/regular-polygon-pts 100 3)
                 (map #(utils/rotate-pt % deg)))
        rand-pt (fn [] [(- 100 (rand-int 200))
                        (- 100 (rand-int 200))])
        pts (repeatedly 1400 rand-pt)]
    (el/g
     (-> (el/polygon tri)
         (tf/style {:fill "none" :stroke "black"}))
     (into [:g {}]
           (for [pt pts]
             (if (utils/pt-inside? tri pt)
               (-> (el/circle 2)
                   (tf/translate pt)
                   (tf/style {:fill "green"}))
               (-> (el/circle 2)
                   (tf/translate pt)
                   (tf/style {:fill "red"}))))))))

(def remove-colinears-example
  (let [pts (-> (p/regular-polygon-pts 100 4)
                (p/simplify 160))
        xpts (p/remove-colinears pts)]
    (el/g
     (into [:g {}]
           (for [pt pts]
             (-> (el/circle 1.5)
                 (tf/translate pt)
                 (tf/style {:fill "black"}))))
     (into [:g {}]
           (for [pt xpts]
             (-> (el/circle 2.5)
                 (tf/translate pt)
                 (tf/style {:fill "green"})))))))

(defn draw-triangulation
  [{:keys [tris]}]
  (into [:g {}]
        (for [tri tris]
          (-> (el/polygon tri)
              (tf/style {:fill "none" :stroke "black"})))))

(def concave
  (let [f (p/blend (p/circle 100)
                   (p/polygon (p/regular-polygon-pts 100 6)) 2)]
    (map f (range 0 1 0.01))))

(def ear-clip-example
  (draw-triangulation (alg/clip-ears concave)))

(def examples [(inside-pt-example)
               (inside-pt-example)
               remove-colinears-example
               ear-clip-example])

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
 "examples/algorithms.html"
 (html 
  [:html 
   [:body
    [:h1 "Algorithms Examples"]
    doc]]))
