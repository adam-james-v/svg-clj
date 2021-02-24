(ns svg-clj.path
  (:require [clojure.string :as st]
            [clojure.spec.alpha :as s]
            [svg-clj.utils :as utils]
            [svg-clj.specs :as specs]))

(defn path
  "Wraps a path string `d` in a hiccup-style data structure.
  The path string is minimally evaluated and is otherwise untouched. Users should consider the function `polygon-path` for constructing paths from points. More complex paths can be built by combining paths with the function `merge-paths`"
  [d]
  {:pre [(s/valid? :svg-clj.specs/path-string d)]}
  [:path {:d d
          :fill-rule "evenodd"}])

(defn- path-command-strings
  "Split the path string `ps` into a vector of path command strings."
  [ps]
  {:pre [(s/valid? :svg-clj.specs/path-string ps)]}
  (-> ps
      (st/replace #"\n" " ")
      (st/split #"(?=[A-DF-Za-df-z])")
      (#(map st/trim %))
      (#(filter (complement empty?) %))))

(defn- relative?
  "True if the path command string `cs` has a relative coordinate command.
  Relative coordinate commands are lowercase.
  Absolute coordinate commands are uppercase."
  [cs]
  {:pre [(s/valid? :svg-clj.specs/command-string cs)]}
  (let [csx (first (st/split cs #"[a-z]"))]
    (not (= cs csx))))

(defn- coord-sys-key
  "Returns the command string `cs`'s coord. system key.
  Key is either :rel or :abs."
  [cs]
  {:pre [(s/valid? :svg-clj.specs/command-string cs)]}
  (if (relative? cs) :rel :abs))

(defn- command-input
  [cs]
  {:pre [(s/valid? :svg-clj.specs/command-string cs)]}
  (let [i (st/split cs #"[A-DF-Za-df-z]")]
    (when (not (empty? (rest i)))
      (apply utils/s->v (rest i)))))

(defn- command
  "Transforms a command string `cs` into a map."
  [cs]
  {:pre [(s/valid? :svg-clj.specs/command-string cs)]}
  {:command  (st/upper-case (re-find #"[A-DF-Za-df-z]" cs))
   :coordsys (coord-sys-key cs)
   :input (command-input cs)})

(defn path-string->commands
  "Turns path string `ps` into a list of its command maps."
  [ps]
  {:pre [(s/valid? :svg-clj.specs/path-string ps)]}
  (->> ps
       (path-command-strings)
       (map command)))

(defn- convert-vh
  [[pcmd ccmd]]
  {:pre [(s/valid? :svg-clj.specs/command-map pcmd)
         (s/valid? :svg-clj.specs/command-map ccmd)]}
  (if (and (not (specs/any-vh? [pcmd])) ;;prev. cmd must NOT be VH
           (specs/any-vh? [ccmd])) ;; curr. cmd must be VH
    (let [[px py] (take-last 2 (:input pcmd))
          vh (:command ccmd)
          xinput (cond (= vh :hline) [(first (:input ccmd)) py]
                       (= vh :vline) [px (first (:input ccmd))])
          ncmd (-> ccmd
                   (assoc :command :line)
                   (assoc :input xinput))]
      [pcmd ncmd])
    [pcmd ccmd]))

(defn- convert-first-vh-cmd
  [cmds]
  {:pre [(s/valid? (s/coll-of :svg-clj.specs/command-map) cmds)]}
  (let [icmd (first cmds)]
  (cons icmd 
        (->> cmds
             (partition 2 1)
             (map convert-vh)
             (map second)))))

(defn- vh->l
  [cmds]
  {:pre [(s/valid? (s/coll-of :svg-clj.specs/command-map) cmds)]}
  (let [iters (iterate convert-first-vh-cmd cmds)]
    (->> iters
         (partition 2 1)
         (take-while (fn [[a b]] (not= a b)))
         last
         last)))

(defn- cmd->path-string
  [{:keys [:command :coordsys :input] :as cmd}]
  {:pre [(s/valid? :svg-clj.specs/command-map cmd)]}
  (let [c (if (= coordsys :abs)
            command
            (st/lower-case command))]
    (str c (apply str (interpose " " input)))))

(defn cmds->path-string
  [cmds]
  {:pre [(s/valid? (s/coll-of :svg-clj.specs/command-map) cmds)]}
  (apply str (interpose " " (map cmd->path-string cmds))))

(defn merge-paths
  "Merges a list of path elements together, keeping props from last path in the list."
  [& paths]
  {:pre [(s/valid? (s/coll-of :svg-clj.specs/path-element) paths)]}
  (let [props (second (last paths))
        d (apply str (interpose " " (map #(get-in % [1 :d]) paths)))]
    [:path (assoc props :d d)]))

(defn- pt->l
  [pt]
  {:pre [(s/valid? :svg-clj.specs/pt2d pt)]}
  {:command "L"
   :coordsys :abs
   :input (vec pt)})

(defn- pt->m
  [pt]
  {:pre [(s/valid? :svg-clj.specs/pt2d pt)]}
  {:command "M"
   :coordsys :abs
   :input (vec pt)})

(defn polygon-path
  [pts]
  {:pre [(s/valid? :svg-clj.specs/pts pts)]}
  (let [open (pt->m (first pts))
        close {:command "Z"
               :coordsys :abs
               :input nil}]
    (-> (map pt->l (rest pts))
        (conj open)
        vec
        (conj close)
        cmds->path-string
        path)))
