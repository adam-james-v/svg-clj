(ns svg-clj.utils
  (:require [clojure.string :as st]
            #?(:cljs
               [cljs.reader :refer [read-string]])))

;; vector arithmetic helpers
(def v+ (partial mapv +))
(def v- (partial mapv -))
(def v* (partial mapv *))

;; simple calcs
(defn to-deg
  [rad]
  (* rad (/ 180 Math/PI)))

(defn to-rad
  [deg]
  (* deg (/ Math/PI 180)))

(defn round
  [num places]
  (let [d (Math/pow 10 places)]
    (/ (Math/round (* num d)) d)))

(defn average
  [& numbers]
  (let [n (count numbers)]
    (/ (apply + numbers) n)))

;; some string transformation tools
(defn v->s
  "Turns the vector `v` into a string formatted for use in SVG attributes."
  [v]
  (apply str (interpose "," v)))

(defn s->v
  "Turns a string of comma or space separated numbers into a vector."
  [s]
  (-> s
      (st/trim)
      (st/split #"[, ]")
      (#(filter (complement empty?) %))
      (#(mapv read-string %))))

(defn xf-kv->str
  [[k v]]
  (str (symbol k) (apply list v)))

(defn str->xf-kv
  [s]
  (let [split (st/split s #"\(")
        key (keyword (first split))
        val (vec (read-string (str "(" (second split))))]
    [key val]))

(defn xf-map->str
  [m]
  (apply str (interpose "\n" (map xf-kv->str m))))

(defn str->xf-map
  [s]
  (if-let [s s]
    (into {} (map str->xf-kv (st/split-lines s)))
    {}))
