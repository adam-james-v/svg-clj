(ns svg-clj.algorithms
  (:require [svg-clj.utils :as utils]
            [clojure.set :as set]))

(def abs #?(:clj #(Math/abs %) :cljs js/Math.abs))
(def pow #?(:clj #(Math/pow %1 %2) :cljs js/Math.pow))

;; https://gist.github.com/mutoo/5617691
(defn circumscribe-tri
  [[[ax ay] [bx by] [cx cy]]]
  (let [A (- bx ax)
        B (- by ay)
        C (- cx ax)
        D (- cy ay)
        E (+ (* A (+ ax bx)) (* B (+ ay by)))
        F (+ (* C (+ ax cx)) (* D (+ ay cy)))
        G (* 2 (- (* A (- cy by)) (* B (- cx bx))))]
    (when (> (abs G) 0.000001)
      (let [cx (/ (- (* D E) (* B F)) G)
            cy (/ (- (* A F) (* C E)) G)
            dx (- cx ax)
            dy (- cy ay)
            r  (+ (pow dx 2) (pow dy 2))]
        {:x cx :y cy :radius-squared r}))))

(defn edges [pts]
  (partition 2 1 (conj (vec pts) (first pts))))

(defn contains-pt?
  [{:keys [x y radius-squared]} [px py]]
  (let [distance-squared (+ (pow (- x px) 2) (pow (- y py) 2))]
    (< distance-squared radius-squared)))

(defn outer-edges
  [tris]
  (let [all-edges (mapcat edges tris)
        matches (fn [edge] (filter #{edge (reverse edge)} all-edges))
        appears-once (fn [edge] (= (count (matches edge)) 1))]
    (filter appears-once all-edges)))

(defn make-new-tris
  [containers pt]
  (->> containers
       outer-edges
       (map (fn [[p1 p2]] [p1 p2 pt]))
       set))

(defn add-pt-to-tris
  [tris pt]
  (let [containers (filter #(contains-pt? (circumscribe-tri %) pt) tris)
        new-tris (make-new-tris containers pt)]
    (set/union (set/difference tris containers) new-tris)))

;; http://paulbourke.net/papers/triangulate/
(defn triangulate
  [pts]
  (let [pts (map (fn [[x y]] [(float x) (float y)]) pts)
        [bl br tr tl] (map #(utils/v* % [2 2]) (utils/bounds-of-pts pts))
        initial #{[tl tr bl] [bl tr br]}
        with-bounds (reduce add-pt-to-tris initial pts)
        tris (remove #(some #{tl tr bl br} %) with-bounds)]
    {:pts pts
     :tris tris
     :edges (distinct (mapcat edges tris))}))

(defn hull
  ([pts] (hull [{:pt (first (sort-by first pts))}] pts))
  ([acc pts]
   (if (or
        ;; stop the process if acc grows larger than the pts count
        (> (count acc) (count pts))
        ;; *should* always end where the last added point closes the poly
        (and (< 1 (count acc))
             (= (:pt (first acc)) (:pt (last acc)))))
     (map :pt (drop-last acc))
     (let [prev (:pt (last acc))
           dir (if (= 1 (count acc))
                 (utils/v+ [0 1] prev)
                 (:pt (last (drop-last acc))))
           f (fn [pt]
               (let [a (when (= 3 (count (into #{} [dir prev pt])))
                         (utils/angle-from-pts dir prev pt))]
                 {:pt pt :angle a}))
           sorted (->> (map f pts)
                       (remove #(nil? (:angle %)))
                       (sort-by #(Math/abs (- (:angle %) 180))))]
       (recur (conj acc (first sorted)) pts)))))

(defn nested-hull
  ([pts] (nested-hull [] pts))
  ([acc pts]
   (if (> 3 (count pts))
     acc
     (let [hull (hull pts)
           npts (remove (set hull) pts)]
       (recur (conj acc hull) npts)))))
