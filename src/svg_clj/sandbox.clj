(ns svg-clj.sandbox
  (:require [clojure.string :as st]
            [clojure.data.xml :as xml]
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
