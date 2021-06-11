(ns svg-clj.main
  (:require [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [hiccup.core :refer [html]]
            [svg-clj.composites :as cp :refer [svg]]
            [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.layout :as lo]
            [svg-clj.tools :as tools]
            [sci.core :as sci])
  (:gen-class))

(def cli-options
  [["-i" "--infile FNAME" "The file to be compiled."
    :default nil]
   ["-o" "--outfile FNAME" "The output filename. Valid Extensions: svg, png"
    :default nil]
   ["-h" "--help"]])

#_(def my-ns-map
  {'svg-clj.composites {'svg svg}
   'svg-clj.elements {'circle el/circle}})

(def my-ns-map
  (into {} 
        (map #(vector % (ns-publics %))
             ['svg-clj.composites
              'svg-clj.utils
              'svg-clj.elements
              'svg-clj.path
              'svg-clj.transforms
              'svg-clj.layout
              'svg-clj.tools])))

(defn sci-load-file
  [fname]
  (-> (slurp fname)
      (sci/eval-string {:namespaces my-ns-map})))

(defn -main [& args]
  (let [parsed (cli/parse-opts args cli-options)
        {:keys [:infile :outfile :help]} (:options parsed)
        [in _] (when infile (str/split infile #"\."))
        outfile (if outfile outfile (str in ".svg"))
        [out ext] (str/split outfile #"\.")]
    (cond 
      help
      (do (println "Usage:")
          (println (:summary parsed)))
          
      (nil? infile)
      (println "Please specify an input file")

      (not (contains? #{"svg" "png"} ext))
      (println "Please specify a valid output format. Valid formats: svg, png.")

      :else
      (do 
        (let [result (deref (sci-load-file infile))
              data (if (= :svg (first result)) result (svg result))]
          (do (println (str "Compiling " infile " into " outfile "."))
              (if (= "svg" ext)
                (spit outfile (html data))
                (tools/png! data outfile))
              (println "Success! Have a nice day :)")))))))
