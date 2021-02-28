(ns svg-clj.main
  (:require [clojure.string :as st]
            #?(:clj [clojure.data.xml :as xml])
            [svg-clj.utils :as utils]
            [svg-clj.specs :as specs]
            [svg-clj.transforms :as transforms]
            [svg-clj.path :as path]))

;; thanks to help from walterl and seancorfield on Clojurians Slack
(defn intern-with-meta
  [q-sym]
  (let [sym (symbol (name q-sym))]
    (do (intern *ns* sym (resolve q-sym))
        (alter-meta! (resolve sym) 
                     #(merge % (meta (resolve q-sym)) 
                             {:ns *ns*})))))

(intern-with-meta 'transforms/centroid)
(intern-with-meta 'transforms/bounds)
(intern-with-meta 'transforms/translate)
(intern-with-meta 'transforms/rotate)
(intern-with-meta 'utils/rotate-pt)
(intern-with-meta 'transforms/scale)
(intern-with-meta 'path/path)
(intern-with-meta 'path/merge-paths)
(intern-with-meta 'path/polygon-path)

(defn svg
   "This fn wraps `content` in an SVG container element.
   The SVG container is parameterized by width `w`, height `h`, and scale `sc`."
  [[w h sc] & content]
  [:svg {:width  w
         :height h
          :viewBox (str "0 0 " w " " h)
         :xmlns "http://www.w3.org/2000/svg"}
   (if sc
     [:g {:transform (str "scale(" sc ")")} content]
     content)])

(defn circle
  [r]
  [:circle {:cx 0 :cy 0 :r r}])

(defn ellipse
  [rx ry]
  [:ellipse {:cx 0 :cy 0 :rx rx :ry ry}])

(defn line
  [[ax ay] [bx by]]
  [:line {:x1 ax :y1 ay :x2 bx :y2 by}])

(defn polygon
  [pts]
  [:polygon {:points (apply str (interpose " " (map utils/v->s pts)))}])

(defn polyline
  [pts]
  [:polyline {:points (apply str (interpose " " (map utils/v->s pts)))}])

(defn rect
  [w h]
  [:rect {:width w :height h :x (/ w -2.0) :y (/ h -2.0)}])

(defn image
  [url w h]
  [:image {:href url :width w :height h :x (/ w -2.0) :y (/ h -2.0)}])

(defn g
  [& content]
  (if (and (= 1 (count content))
           (not (keyword? (first (first content)))))
    ;; content is a list of a list of elements
    (into [:g {}] (first content))
    ;; content is a single element OR a list of elements
    (into [:g {}] (filter (complement nil?) content))))

(defn text
  [text]
  [:text {:x 0 :y 0} text])

#?(:clj
   (defn xml->hiccup
     [xml]
     (if-let [t (:tag xml)]
       (let [elem [t]
             elem (if-let [attrs (:attrs xml)]
                    (conj elem attrs)
                    elem)]
         (into elem (map xml->hiccup (:content xml))))
       xml)))

#?(:clj
   (defn ->edn
     [str]
     (->> (xml/parse-str str 
                         :skip-whitespace true
                         :namespace-aware false)
          xml->hiccup
          #_(tree-seq vector? rest)
          #_(filter vector?)
          #_(filter #(= :svg (first %)))
          #_first)))

#?(:clj
   (defn unwrap-elements
     [edn]
     (filter specs/element? edn)))

(defn style
  [style [k props & content]]
  (into [k (merge props style)] content))
