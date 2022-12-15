(ns svg-clj.jvm-utils
  (:require [clojure.string :as str]
            [clojure.zip :as zip]
            [clojure.data.xml :as xml]
            #?(:cljs [cljs.reader :refer [read-string]])))

(defn str->number
  "Turns a string `s` into a number if possible, otherwise returns `s`."
  [s]
  (let [n (try (read-string s)
               (catch #?(:clj Exception
                         :cljs js/Object) _ s))]
    (if (number? n) n s)))

(def numerical-attrs
  "Set of SVG attributes which have numerical values."
  #{;; circle, ellipse
    :cx :cy :r :rx :ry
    ;; image, rect
    :width :height :x :y
    ;; line
    :x1 :y1 :x2 :y2})

(defn cast-numerical-attrs
  "Casts certain attribute values to numbers if they are strings.
Attributes to be cast are defined in `numerical-attrs` and include `:cx`, `:cy`, `:width`, etc."
  [attrs]
  (if (empty? attrs)
    {}
    (apply merge
           (map
            (fn [[k v]]
              (if (numerical-attrs k)
                {k (str->number v)}
                {k v}))
            attrs))))

(defn- fix-ns-tag
  [t]
  (let [namespace (namespace t)
        name (name t)]
    (if namespace
      (-> namespace
          (str/split #"\.")
          first
          (str ":" name)
          keyword)
      t)))

(defn xml->hiccup
  "Convert XML to hiccup."
  [xml]
  (if-let [t (:tag xml)]
    (let [elem [(fix-ns-tag t)]
          elem (conj elem (cast-numerical-attrs (:attrs xml)))]
      (into elem (map xml->hiccup (remove string? (:content xml)))))
    xml))

(defn svg-str->hiccup
  "Parses an SVG string into a Hiccup data structure, keeping all nodes."
  [svg-str]
  (-> svg-str
      (xml/parse-str :namespace-aware false)
      xml->hiccup))
