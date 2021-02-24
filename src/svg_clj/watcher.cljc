(ns svg-clj.watcher
  (:require [clojure.string :as st]
            [svg-clj.main :refer :all]
            [hiccup.core :refer [html]]
            [hawk.core :as hawk]))
 
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
