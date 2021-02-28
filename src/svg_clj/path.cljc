(ns svg-clj.path
  (:require [clojure.string :as st]
            [clojure.spec.alpha :as s]
            [svg-clj.utils :as utils]
            [svg-clj.specs :as specs]))

(defn path
  "Wraps a path string `d` in a hiccup-style data structure.
  The path string is minimally evaluated and is otherwise untouched. Users should consider the function `polygon-path` for constructing paths from points. More complex paths can be built by combining paths with the function `merge-paths`"
  [d]
  [:path {:d d
          :fill-rule "evenodd"}])

(defn- path-command-strings
  "Split the path string `ps` into a vector of path command strings."
  [ps]
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
  (if (relative? cs) :rel :abs))

(defn- command-input
  [cs]
  (let [i (st/split cs #"[A-DF-Za-df-z]")]
    (when (not (empty? (rest i)))
      (apply utils/s->v (rest i)))))

(defn- command
  "Transforms a command string `cs` into a map."
  [cs]
  {:command  (st/upper-case (re-find #"[A-DF-Za-df-z]" cs))
   :coordsys (coord-sys-key cs)
   :input (command-input cs)})

(defn- merge-cursor
  [[pcmd ccmd]]
  (let [cursor (vec (take-last 2 (:input pcmd)))]
    (assoc ccmd :cursor cursor)))

(defn path-string->commands
  "Turns path string `ps` into a list of its command maps."
  [ps]
  (->> ps
       path-command-strings
       (map command)
       (concat [{:command "M"
                 :coordsys :abs
                 :input [0 0]}])
       (partition 2 1)
       (map merge-cursor)))

(defn- convert-vh
  [[pcmd ccmd]]
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
  (let [icmd (first cmds)]
    (cons icmd 
          (->> cmds
               (partition 2 1)
               (map convert-vh)
               (map second)))))

(defn- vh->l
  [cmds]
  (let [iters (iterate convert-first-vh-cmd cmds)]
    (->> iters
         (partition 2 1)
         (take-while (fn [[a b]] (not= a b)))
         last
         last)))

(defn- cmd->path-string
  [{:keys [:command :coordsys :input] :as cmd}]
  (let [c (if (= coordsys :abs)
            command
            (st/lower-case command))]
    (str c (apply str (interpose " " input)))))

(defn cmds->path-string
  [cmds]
  (apply str (interpose " " (map cmd->path-string cmds))))

(defn merge-paths
  "Merges a list of path elements together, keeping props from last path in the list."
  [& paths]
  (let [props (second (last paths))
        d (apply str (interpose " " (map #(get-in % [1 :d]) paths)))]
    [:path (assoc props :d d)]))

(defn- pt->l
  [pt]
  {:command "L"
   :coordsys :abs
   :input (vec pt)})

(defn- pt->m
  [pt]
  {:command "M"
   :coordsys :abs
   :input (vec pt)})

(defn line-path
  [a b]
  (-> [(pt->m a) (pt->l b)]
      cmds->path-string
      path))

(defn polygon-path
  [pts]
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

(defn polyline-path
  [pts]
  (let [open (pt->m (first pts))]
    (-> (map pt->l (rest pts))
        (conj open)
        vec
        cmds->path-string
        path)))

(defn rect-path
  [w h]
  (let [w2 (/ w 2.0)
        h2 (/ h 2.0)]
    (polygon-path [ [(- w2) (- h2)] [w2 (- h2)] 
                    [w2 h2]         [(- w2) h2] ])))

(defn- partial-bezier
  ([a]
   (-> {:command "T"
        :coordsys :abs
        :input (vec a)}
       cmd->path-string))

  ([a b]
   (-> {:command "S"
        :coordsys :abs
        :input (concat a b)}
       cmd->path-string)))

(defn bezier
  ([a b c]
   (let [open (pt->m a)]
     (-> {:command "Q"
          :coordsys :abs
          :input (concat b c)}
         list
         (conj open)
         vec
         cmds->path-string
         path)))

  ([a b c d]
   (let [open (pt->m a)]
     (-> {:command "C"
          :coordsys :abs
          :input (concat b c d)}
         list
         (conj open)
         vec
         cmds->path-string
         path))))

(defn- partial-arc
  [rx ry rot laf sw a]
  (let [open (pt->m a)]
    (-> {:command "A"
         :coordsys :abs
         :input (concat [rx ry rot laf sw] a)}
        cmd->path-string)))

(defn- build-arc
  [rx ry rot laf sw a b]
  (let [open (pt->m a)]
    (-> {:command "A"
         :coordsys :abs
         :input (concat [rx ry rot laf sw] b)}
        list
        (conj open)
        vec
        cmds->path-string
        path)))

(defn arc
  [a ctr deg]
  (let [r (utils/distance a ctr)
        angle 0
        b (utils/rotate-pt-around-center deg ctr a)
        laf (if (<= deg 180) 0 1)]
     (build-arc r r angle laf 1 a b)))

(defn circle-path
  [r]
  (let [open (pt->m [r 0])
        close {:command "Z"
               :coordsys :abs
               :input nil}]
    (-> [open
         {:command "A"
          :coordsys :abs
          :input [r r 0 1 0 (- r) 0]}
         {:command "A"
          :coordsys :abs
          :input [r r 0 1 0 r 0]}
         close]
        cmds->path-string
        path)))

(defn ellipse-path
  [rx ry]
  (let [open (pt->m [rx 0])
        close {:command "Z"
               :coordsys :abs
               :input nil}]
    (-> [open
         {:command "A"
          :coordsys :abs
          :input [rx ry 0 1 0 (- rx) 0]}
         {:command "A"
          :coordsys :abs
          :input [rx ry 0 1 0 rx 0]}
         close]
        cmds->path-string
        path)))
