(ns examples.algorithms
  (:require [hiccup.core :refer [html]]
            [svg-clj.algorithms :as alg]
            [svg-clj.composites :as comp :refer [svg]]
            [svg-clj.elements :as el]
            [svg-clj.parametric :as p]
            [svg-clj.transforms :as tf]
            [svg-clj.utils :as u]))

(defn inside-pt-example
  []
  (let [deg (rand-int 361)
        tri (->> (p/regular-polygon-pts 100 3)
                 (map #(u/rotate-pt % deg)))
        rand-pt (fn [] [(- 100 (rand-int 200))
                        (- 100 (rand-int 200))])
        pts (repeatedly 1400 rand-pt)]
    (el/g
     (-> (el/polygon tri)
         (tf/style {:fill "none" :stroke "black"}))
     (into [:g {}]
           (for [pt pts]
             (if (u/pt-inside? tri pt)
               (-> (el/circle 2)
                   (tf/translate pt)
                   (tf/style {:fill "green"}))
               (-> (el/circle 2)
                   (tf/translate pt)
                   (tf/style {:fill "red"}))))))))

(def remove-colinears-example
  (let [pts (-> (p/regular-polygon-pts 100 4)
                (p/simplify 160))
        xpts (alg/remove-colinears pts)]
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
