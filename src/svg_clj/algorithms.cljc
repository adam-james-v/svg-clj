(ns svg-clj.algorithms
  (:require [clojure.set :as set]
            [svg-clj.utils :as u]))

;; todo: check if we need this fn here.
(def pow
  "Raise the first number to the power of the second number."
  #?(:clj #(Math/pow %1 %2) :cljs js/Math.pow))

;; https://gist.github.com/mutoo/5617691
(defn- circumscribe-tri
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

(defn- edges
  [pts]
  (partition 2 1 (conj (vec pts) (first pts))))

(defn- contains-pt?
  [{:keys [x y radius-squared]} [px py]]
  (let [distance-squared (+ (pow (- x px) 2) (pow (- y py) 2))]
    (< distance-squared radius-squared)))

(defn- outer-edges
  [tris]
  (let [all-edges (mapcat edges tris)
        matches (fn [edge] (filter #{edge (reverse edge)} all-edges))
        appears-once (fn [edge] (= (count (matches edge)) 1))]
    (filter appears-once all-edges)))

(defn- make-new-tris
  [containers pt]
  (->> containers
       outer-edges
       (map (fn [[p1 p2]] [p1 p2 pt]))
       set))

(defn- add-pt-to-tris
  [tris pt]
  (let [containers (set (filter #(contains-pt? (circumscribe-tri %) pt) tris))
        new-tris (make-new-tris containers pt)]
    (set/union (set/difference tris containers) new-tris)))

;; http://paulbourke.net/papers/triangulate/
;; todo: Test this a bunch, see if I can get it to work with any arbitrary set of pts.
;; todo: see where it fails (eg. concave pt sets?) and fix
(defn delaunay
  "Compute a delaunay triangulation of `pts`."
  [pts]
  (let [pts (map (fn [[x y]] [(float x) (float y)]) pts)
        pt-indices (zipmap pts (range 0 (count pts)))
        [bl br tr tl] (map #(u/v* % [2 2]) (u/bounds-of-pts pts))
        initial #{[tl tr bl] [bl tr br]}
        with-bounds (reduce add-pt-to-tris initial pts)
        tris (remove #(some #{tl tr bl br} %) with-bounds)
        tri-indices (fn [tri] (mapv #(get pt-indices %) tri))]
    {:pts pts
     :tris tris
     :tri-indices (map tri-indices tris)
     :edges (distinct (mapcat edges tris))}))

(defn remove-colinears
  "Removes all points from `pts` which are colinear with other corners from the list of points `pts`.
  That is, any point that does not itself form a corner is removed."
  [pts]
  (let [indices (zipmap pts (range (count pts)))
        tris (partition 3 1 (concat pts (take 2 pts)))
        clpts (set (map second (filter #(apply u/colinear? %) tris)))
        xindices (vals (apply dissoc indices clpts))]
    (map #(get pts %) xindices)))

(defn- clip-one-ear
  [pts]
  (let [pts (vec pts)
        indices (zipmap pts (range (count pts)))
        corners (->> pts
                     (#(concat % (take 2 %)))
                     (partition 3 1)
                     (filter #(#{:convex} (apply u/corner-condition %))))
        clear? (fn [corner]
                 (not (seq (filter #(u/pt-inside? corner %) pts))))
        tri (first (filter clear? corners))]
    {:pts pts
     :npts (mapv #(get pts %) (sort (vals (dissoc indices (second tri)))))
     :tri tri}))

(defn clip-ears
  "Create a tessellation of the polygon defined by `pts`.
  This algorithm works acceptably, but returns 'bad' tessellations often. That is, it will return a mesh with very thin triangles, it will slow down if there are many many points."
  ([pts] (clip-ears {:indices (zipmap pts (range (count pts)))} pts [] []))
  ([data pts tris indices]
   (if (< (count pts) 3)
     (merge data {:tris tris :tri-indices indices})
     (let [{:keys [npts tri]} (clip-one-ear pts)
           local-indices (mapv #(get (:indices data) %) tri)]
       (recur data npts (conj tris tri) (conj indices local-indices))))))

(defn hull
  "Compute the convex hull of `pts`."
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
                 (u/v+ [0 1] prev)
                 (:pt (last (drop-last acc))))
           f (fn [pt]
               (let [a (when (= 3 (count (into #{} [dir prev pt])))
                         (u/angle-from-pts dir prev pt))]
                 {:pt pt :angle a}))
           sorted (->> (map f pts)
                       (remove #(nil? (:angle %)))
                       (sort-by #(abs (- (:angle %) 180))))]
       (recur (conj acc (first sorted)) pts)))))

(defn nested-hull
  "Compute the nested convex hull of `pts`."
  ([pts] (nested-hull [] pts))
  ([acc pts]
   (if (> 3 (count pts))
     acc
     (let [hull (hull pts)
           npts (remove (set hull) pts)]
       (recur (conj acc hull) npts)))))
