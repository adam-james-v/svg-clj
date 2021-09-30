(ns svg-clj.tools-test
  (:require [clojure.string :as str]
            [svg-clj.utils :as utils]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.elements :as el]
            [svg-clj.composites :refer [svg]]
            [svg-clj.tools :as tools]))

(def sk (slurp "examples/load-svg-test.svg"))
(def loaded-sk (tools/load-svg sk-url))
(def sk-elems (tools/get-elems loaded-sk))
