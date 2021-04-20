(ns svg-clj.watcher
  (:require [clojure.string :as st]
            [hiccup2.core :refer [html]]
            [hawk.core :as hawk]
            [svg-clj.main :refer [circle
                                  ellipse
                                  g
                                  image
                                  line
                                  polygon
                                  polyline
                                  rect
                                  style
                                  svg
                                  text]]))

(defn design-watch
  [f]
  (hawk/watch!
   [{:paths [f]
     :handler
     (fn [ctx e]
       (require '[svg-clj.main :refer :all]
                '[hiccup.core :refer [html]])
       (->> (slurp f)
            (format "[%s]")
            load-string
            (filter (complement var?))
            html
            (spit "test.html"))
       ctx)}]))

(defn -main [& args] (design-watch (first args)))
