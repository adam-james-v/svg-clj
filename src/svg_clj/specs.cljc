(ns svg-clj.specs
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))
            
(s/def ::pt2d (s/tuple number? number?))
(s/def ::pts (s/coll-of ::pt2d))

(def svg-element-keys
  "SVG elements provided by the library."
  #{:circle
    :ellipse
    :line
    :path
    :polygon
    :polyline
    :rect
    :text
    :image
    :g})

(s/def ::basic-element
  (s/cat :tag svg-element-keys
         :props map?))

(s/def ::text-element
  (s/cat :tag #{:text}
         :props map?
         :content string?))

(s/def ::g-element
  (s/cat :tag #{:g}
         :props map?
         :content (s/* ::svg-element)))

(s/def ::svg-element
  (s/or :basic (s/spec ::basic-element)
        :text (s/spec ::text-element)
        :group (s/spec ::g-element)))

(s/def ::path-element
  (s/cat :tag #{:path}
         :props (s/keys :req-un [::d]) 
         :content (s/* ::svg-element)))

(defn pt2d? [a] (s/valid? ::pt2d a))
(defn pts? [s] (s/valid? ::pts s))

(defn element?
  "Checks if `elem` is an SVG element."
  [elem]
  (s/valid? ::svg-element elem))

(defn path-string-allowed? 
  [string] 
  (empty? (str/replace string #"[MmZzLlHhVvCcSsQqTtAaeE0-9-,.\s]" "")))

(defn path-string-valid-syntax?
  [string]
  (nil? (re-find #"[a-zA-Z][a-zA-Z]" string)))

(defn path-string-valid-start?
  [string]
  (nil? (re-find #"^[0-9-,.]" string)))

(defn path-string-valid-end?
  [string]
  (nil? (re-find #".*[-,.]$" string)))

(defn path-string-single-command?
  [string]
  (= 1 (count (re-seq #"[A-DF-Za-df-z]" string))))

(s/def ::path-string
  (s/and string?
         (complement empty?)
         path-string-allowed?
         path-string-valid-syntax?
         path-string-valid-start?
         path-string-valid-end?
         (complement path-string-single-command?)))

(s/def ::command-string
  (s/and string?
         (complement empty?)
         path-string-allowed?
         path-string-valid-syntax?
         path-string-valid-start?
         path-string-valid-end?
         path-string-single-command?))

(def commands #{"M" "L" "H" "V" "C" "S" "Q" "T" "A" "Z"})
(s/def ::command commands)
(s/def ::coordsys #{:rel :abs})
(s/def ::input (s/or :data (s/+ number?)
                     :nil nil?))
(s/def ::command-map
  (s/keys :req-un [::command ::coordsys ::input]))

(s/def ::bounds
  (s/tuple ::pt2d ::pt2d ::pt2d ::pt2d))

(s/fdef path
  :args (s/cat :d ::path-string)
  :ret ::path-element)

(s/fdef path-command-strings
  :args (s/cat :path-string ::path-string)
  :ret (s/coll-of ::command-string))

(s/fdef command
  :args (s/cat :command-string ::command-string)
  :ret ::command-map)

(s/fdef path-string->commands
  :args (s/cat :path-string ::path-string)
  :ret (s/coll-of ::command-map))

(s/fdef vh->l
  :argrs (s/cat :commands (s/coll-of ::command-map))
  :ret (complement any-vh?))

(s/fdef merge-paths
  :args (s/cat :paths (s/coll-of ::path-element))
  :ret ::path-element)

(s/fdef polygon-path
  :args (s/cat :pts ::pts)
  :ret ::path-element)

(s/fdef centroid-of-pts
  :args (s/cat :pts ::pts)
  :ret ::pt2d)

(s/fdef centroid
  :args (s/or :one (s/coll-of ::svg-element)
              :many (s/coll-of (s/+ ::svg-element)))
  :ret ::pt2d)

(s/fdef bounds
  :args (s/cat :elems (s/coll-of ::svg-element))
  :ret ::bounds)

(s/fdef circle
  :args (s/cat :r number?)
  :ret ::svg-element)

(s/fdef ellipse
  :args (s/cat :rx number? :ry number?)
  :ret ::svg-element)

(s/fdef line
  :args (s/cat :a ::pt2d :b ::pt2d)
  :ret ::svg-element)

(s/fdef polygon
  :args (s/cat :pts ::pts)
  :ret ::svg-element)

(s/fdef polyline
  :args (s/cat :pts ::pts)
  :ret ::svg-element)

(s/fdef rect
  :args (s/cat :w number? :h number?)
  :ret ::svg-element)

(s/fdef image
  :args (s/cat :url string? :w number? :h number?)
  :ret ::svg-element)

#_(s/fdef g
  :args ::groupable
  :ret ::svg-element)

(s/fdef text
  :args (s/cat :text string?)
  :ret ::svg-element)
