(ns examples.bezier
  (:require [hiccup.core :refer [html]]
            [svg-clj.composites :refer [svg]]
            [svg-clj.elements :as el]
            [svg-clj.parametric :as p]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]))

(def pts [ [0 0] [100 -300] [200 -300] [300 0]])
#_(def pts [ [110 150] [25 190] [210 250] [210 30]])

(def ts [0.2 0.4 0.6 0.8])
(def curve (p/bezier pts))
(def curve2 (p/rational-bezier pts [1 2.5 0.5 1]))

(def cols ["blue" "cyan" "purple" "pink" "blue" "skyblue" "slategray" "gold" "orange" "red"])

;; runs slow due to arc-length calc
(def curves
  (let [cpts (p/uniform-split-bezier curve 6) #_(p/multi-split-bezier curve ts)]
    (map-indexed #(el/g
                   (-> (apply path/bezier %2)
                       (tf/style {:fill "none"
                                  :stroke-width "3px"
                                  :stroke (get cols %1)}))
                   (-> (el/circle 5)
                       (tf/translate (last %2))
                       (tf/style {:fill "red"})))
                   cpts)))

(def split-curve (p/uniform-split-bezier curve 4))

(def a (-> (apply path/bezier pts)
           (tf/style {:fill "none"
                      :stroke-width "6px"
                      :stroke "pink"})))

(def aa (-> (p/split-bezier curve 0.2)
            :a
            (#(apply path/bezier %))
            (tf/style {:fill "none"
                       :stroke-width "3px"
                       :stroke "blue"})))

(def ab (-> (p/split-bezier curve 0.2)
            :b
            (#(apply path/bezier %))
            (tf/style {:fill "none"
                       :stroke-width "3px"
                       :stroke "green"})))

(def b
  (let [cpts (map curve2 (range 0 1.01 0.01))]
    (el/g
     (map #(-> (el/circle 3)
               (tf/translate %)
               (tf/style {:fill "red"}))
          cpts))))

(def c (el/g a aa ab b))

(def examples [a
               aa
               ab
               b
               c])

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
 "examples/bezier.html"
 (html
  [:html
   [:body
    [:h1 "Bezier Curve Examples"]
    doc]]))
