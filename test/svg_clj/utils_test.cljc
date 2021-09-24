(ns svg-clj.utils-test
  (:require [svg-clj.utils :as utils]
            [svg-clj.parametric :as p]
            [svg-clj.transforms :as tf]
            [clojure.test :refer [deftest is]]))

(deftest angle-from-pts
  (let [angles (map
                #(utils/angle-from-pts [10 0] [0 0] %)
                (p/regular-polygon-pts 10 20))
        sorted-angles (sort angles)]
    (is (= angles sorted-angles))))
