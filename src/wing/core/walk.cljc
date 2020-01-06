(ns wing.core.walk)

(defn- pathwalk-pre*
  "Private helper for pathwalk-pre"
  [f path form]
  (let [new-val (f path form)]
    (cond
      (map? new-val)    (into {} (map (fn [[k v]]
                                        [k (pathwalk-pre* f (conj path k) v)])) new-val)
      (vector? new-val) (into [] (map-indexed (fn [i v]
                                                (pathwalk-pre* f (conj path i) v))) new-val)
      :else             new-val)))

(defn pathwalk-pre
  "Similar to `clojure.walk/prewalk` but includes the path of the value as the first argument to `f`

  Does not invoke `f` on non-pathable entries (eg. the key of a `map-entry`)"
  [f form]
  (pathwalk-pre* f [] form))

(defn- pathwalk-post*
  "Private helper for pathwalk-pre"
  [f path form]
  (f path
     (cond
       (map? form)    (into {} (map (fn [[k v]]
                                      [k (pathwalk-post* f (conj path k) v)])) form)
       (vector? form) (into [] (map-indexed (fn [i v]
                                              (pathwalk-post* f (conj path i) v))) form)
       :else          form)))

(defn pathwalk-post
  "Similar to `clojure.walk/postwalk` but includes the path of the value as the first argument
  to `f`

  Does not invoke `f` on non-pathable entries (eg. the key of a `map-entry`)"
  [f form]
  (pathwalk-post* f [] form))
