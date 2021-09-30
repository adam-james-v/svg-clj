(ns svg-clj.tools-test
  (:require [clojure.string :as str]
            [svg-clj.utils :as utils]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]
            [svg-clj.elements :as el]
            [svg-clj.composites :refer [svg]]
            [svg-clj.tools :as tools]
            [clojure.test :as test :refer [deftest is]]))

(def loaded-sk (tools/load-svg "examples/load-svg-test.svg"))
(def sk-elems (tools/load-svg-elems "examples/load-svg-test.svg"))
(def circle-elems (tools/load-svg-elems "examples/load-svg-test.svg" #{:circle}))

(deftest basic-loading-test
  (is (= :svg (first loaded-sk)))
  (is (= :g (first (first sk-elems))))
  (is (= 10 (count sk-elems)))
  (is (= 2 (count circle-elems)))
  (is (= #{:circle} (set (map first circle-elems)))))
