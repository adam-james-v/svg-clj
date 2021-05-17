(s/fdef circle
  :args (s/cat :r number?)
  :ret ::svg-element)

(s/fdef ellipse
  :args (s/cat :rx number? :ry number?)
  :ret ::svg-element)

(s/fdef line
  :args (s/cat :a ::pt2d :b ::pt2d)
  :ret ::svg-element)

(s/fdef polygon
  :args (s/cat :pts ::pts)
  :ret ::svg-element)

(s/fdef polyline
  :args (s/cat :pts ::pts)
  :ret ::svg-element)

(s/fdef rect
  :args (s/cat :w number? :h number?)
  :ret ::svg-element)

(s/fdef image
  :args (s/cat :url string? :w number? :h number?)
  :ret ::svg-element)

#_(s/fdef g
  :args ::groupable
  :ret ::svg-element)

(s/fdef text
  :args (s/cat :text string?)
  :ret ::svg-element)

(s/fdef path
  :args (s/cat :d ::path-string)
  :ret ::path-element)

(s/fdef path-command-strings
  :args (s/cat :path-string ::path-string)
  :ret (s/coll-of ::command-string))

(s/fdef command
  :args (s/cat :command-string ::command-string)
  :ret ::command-map)

(s/fdef path-string->commands
  :args (s/cat :path-string ::path-string)
  :ret (s/coll-of ::command-map))

(s/fdef vh->l
  :argrs (s/cat :commands (s/coll-of ::command-map))
  :ret (complement any-vh?))

(s/fdef merge-paths
  :args (s/cat :paths (s/coll-of ::path-element))
  :ret ::path-element)

(s/fdef polygon-path
  :args (s/cat :pts ::pts)
  :ret ::path-element)

(s/fdef centroid-of-pts
  :args (s/cat :pts ::pts)
  :ret ::pt2d)

(s/fdef centroid
  :args (s/or :one (s/coll-of ::svg-element)
              :many (s/coll-of (s/+ ::svg-element)))
  :ret ::pt2d)

(s/fdef bounds
  :args (s/cat :elems (s/coll-of ::svg-element))
  :ret ::bounds)
