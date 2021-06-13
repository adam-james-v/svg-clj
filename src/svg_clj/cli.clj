(ns svg-clj.cli
  (:require [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [hiccup.core :refer [html]]
            [hawk.core :as hawk]
            [svg-clj.composites :as cp :refer [svg]]
            [svg-clj.utils :as utils]
            [svg-clj.elements :as el]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.layout :as lo]
            [sci.core :as sci])
  (:gen-class))

(def my-ns-map
  (into {} 
        (map #(vector % (ns-publics %))
             ['svg-clj.composites
              'svg-clj.utils
              'svg-clj.elements
              'svg-clj.path
              'svg-clj.transforms
              'svg-clj.layout])))

(defn sci-load-file
  [fname]
  (-> (slurp fname)
      (sci/eval-string {:namespaces my-ns-map})))

(defn watch!
  [infile outfile]
  (let [ [name ext] (str/split infile #"\.")]
    (hawk/watch!
     [{:paths [infile]
       :handler
       (fn [ctx e]
         (let [result (deref (sci-load-file infile))
               data (if (= :svg (first result)) result (svg result))
               msg (str "| Compiling " infile " into " outfile ". |")]
           (println (apply str (repeat (count msg) "-")))
           (println msg)
           (println (apply str (repeat (count msg) "-")))
           (spit outfile (html data))
           (println "Done. Waiting for changes")
           ctx))}])))

(def cli-options
  [["-i" "--infile FNAME" "The file to be compiled."
    :default nil]
   ["-o" "--outfile FNAME" "The output filename. Valid Extensions: svg"
    :default nil]
   ["-w" "--watch" "Watch the file for changes and re-compile on change."
    :default false]
   ["-h" "--help"]])

(defn -main [& args]
  (let [parsed (cli/parse-opts args cli-options)
        {:keys [:infile :outfile :watch :help]} (:options parsed)
        [in _] (when infile (str/split infile #"\."))
        outfile (if outfile outfile (str in ".svg"))
        [out ext] (str/split outfile #"\.")]
    (cond
      help
      (do (println "Usage:")
          (println (:summary parsed)))
          
      (nil? infile)
      (println "Please specify an input file")

      (not (contains? #{"svg"} ext))
      (println "Please specify a valid output format. Valid formats: svg.")

      watch
      (do (println (str "Waiting for changes to " infile "."))
          (watch! infile outfile))
      
      :else
      (let [result (deref (sci-load-file infile))
            data (if (= :svg (first result)) result (svg result))
            msg (str "| Compiling " infile " into " outfile ". |")]
        (println (apply str (repeat (count msg) "-")))
        (println msg)
        (println (apply str (repeat (count msg) "-")))
        (spit outfile (html data))
        (println "Success! Have a nice day :)")))))
