(ns wing.core
  "Namespace providing extensions to clojure's standard library"
  (:require [clojure.core :as core]
            [wing.core.walk :as w.walk])
  (:refer-clojure :exclude [rand group-by random-uuid]))

(defn- deep-merge*
  "Recursive helper function for deep-merge. Keeps the right-most value
  within nested maps."
  [a b]
  (if (and (map? a) (map? b))
    (merge-with deep-merge* a b)
    b))

(defn random-uuid
  "Return a random-uuid. Cljc port of the clojurescript variant

  Now deprecated. Use `clojure.core/random-uuid` instead."
  {:deprecated true}
  []
  #?(:cljs (clojure.core/random-uuid)
     :clj (clojure.core/random-uuid)))

(defn deep-merge
  "Similar to merge, but nested maps will be merged.

  If any non-nested map value is duplicated, the right associated value will be
  kept. This includes scenarios where a left-bound map is merged with a
  right-bound primitive."
  [x & [y & maps]]
  (if-not y
    x
    (recur (deep-merge* x y) maps)))

(defn apply-if
  "Conditionally applies `(f x)` if `pred` returns `true`, otherwise returns `x`."
  [x pred f]
  (if (pred x)
    (f x)
    x))

(defn apply-when
  "Conditionally applies `(f x)` if `pred` returns `true`, otherwise returns nil"
  [x pred f]
  (when (pred x)
    (f x)))

(defn guard
  "Return `nil` unless `(pred x)`, then return `x`.

  The unary form returns a partially applied `guard`.

  Useful for `condp`."
  ([pred] (partial guard pred))
  ([pred x] (when (pred x) x)))

(defn update-if-exists
  "Like `clojure.core/update`, but only calls `f` if the `k` exists
  in the `m`."
  [m & {:as kfns}]
  (into m (for [[k v] (select-keys m (keys kfns))]
            [k ((kfns k) v)])))

(defn update-if-some
  "Like `clojure.core/update`, but only calls `f` if the `k` exists
  in the `m` and it has a non-nil value."
  [m & {:as kfns}]
  (into m (for [[k v] (select-keys m (keys kfns)) :when (some? v)]
            [k ((kfns k) v)])))

(defn binary-partition
  "Return a tuple of `coll` with the first element being for which `pred` returned
  truthy, and the second element being those which returned a falsey value."
  [pred coll]
  (loop [coll coll
         trues nil
         falses nil]
    (cond
      (empty? coll)       [(reverse trues) (reverse falses)]
      (pred (first coll)) (recur (rest coll) (conj trues (first coll)) falses)
      :else               (recur (rest coll) trues (conj falses (first coll))))))


(defn partition-keys
  "Similar to `clojure.core/select-keys` but returns a vector of two maps. The first is
  the map with the selected `keys`, while the other is the original `map` with the `keys` removed."
  [map keys]
  [(select-keys map keys) (apply dissoc map keys)])

(defn extract
  "Returns a tuple of a vector of the vals of `keys` and `map` with `keys` removed.

  Example:

  (extract {:name \"Bob\" :age 42 :weight 200} [:age :weight])
  ;; [[42 200] {:name \"Bob\"}]"
  [map keys]
  (loop [keys    keys
         vals    (transient [])
         new-map (transient map)]
    (if (seq keys)
      (let [k (first keys)
            v (get map k)]
        (recur (rest keys) (conj! vals v) (dissoc! new-map k)))
      [(persistent! vals) (persistent! new-map)])))

(defn sum
  "Variadic function which will sum `args`. Treats nil as 0."
  [& args]
  (reduce + 0 (keep identity args)))

(defn avg
  "Variadic function which will average `args`. Returns nil on empty collections"
  [& args]
  (when (seq args)
    (/ (apply sum args) (count args))))

(defn standard-deviation
  "Varidic function wheich will return the standard deviation of the provided `args`. Returns `nil`
  on empty collections"
  [& args]
  (when-some [mean (apply avg args)]
    (Math/sqrt (apply avg (map #(Math/pow (- % mean) 2) args)))))

(defn round
  "Rounds `n` to the given `precision`"
  ([n] (round n 0))
  ([n precision]
   (let [factor (Math/pow 10 precision)]
     (/ (Math/round (* n factor))
        factor))))


(defn find-first
  "Returns the first item matching `pred` in `coll`.

  Optionally takes an `extract` function which will be applied iff the matching item is not `nil`.

  Example:

  (find-first :primary :email [{:primary false :email \"bob@gmail.com\"}
                              {:primary true :email \"foo@bar.com\"}])
  "
  ([pred coll] (find-first pred identity coll))
  ([pred extract coll]
   (when-some [item (first (filter pred coll))]
     (extract item))))

(defn find-first-ix
  "Return the index of the first item matching `pred` in `coll`"
  [pred coll]
  (->> coll
       (map-indexed vector)
       (find-first (comp pred second) first)))

(defn indistinct
  "Returns elements in a sequence that appear more than once.

  Only returns successive elements that have been seen before.

  If called without a collection, returns a stateful transducer.

  user=> (indistinct [1 2 1 2 2 3 4 5 1])
  (1 2 2 1)"
  ([]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([acc] (rf acc))
         ([acc item]
          (if (@seen item)
            (rf acc item)
            (do (vswap! seen conj item)
                acc)))))))
  ([coll]
   (let [seen (volatile! #{})]
     (->> coll
          (map (fn check-seen [i]
                 (if (@seen i)
                   i
                   (do (vswap! seen conj i)
                       ::drop))))
          (remove #{::drop})))))

(defn rand
  "Behaves just like `clojure.core/rand` but optionally accepts a lower bound."
  ([n] (core/rand n))
  ([low high] (+ low (core/rand (- high low)))))


(defn inspect
  "A function useful for println debugging. Prints the value (and an optional
  message) and returns it."
  ([v]
   (println v)
   v)
  ([msg v]
   (println (str msg ":") v)
   v)
  ([msg f v]
   (println (str msg ":") (f v))
   v))


(defn arg
  "Returns a function which will take any number of arguments and return
  the 1-based index argument of `n`. Mostly useful for composition.

  user=> ((a 1) 1 2 3)
  1"
  [n]
  (fn [& args]
    (nth args (dec n) nil)))


(defn dedupe-by
  "Returns a lazy sequence of the elements of coll, removing any **consecutive**
  elements that return duplicate values when passed to a function f.

  Returns a stateful transducer when no collection is provided.

  See also `distinct-by`"
  ([f]
   (fn [rf]
     (let [pv (volatile! ::none)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result x]
          (let [prior @pv
                fx    (f x)]
            (vreset! pv fx)
            (if (= prior fx)
              result
              (rf result x))))))))
  ([f coll]
   (sequence (dedupe-by f) coll)))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll with elements that return non-distinct
  values from `f` removed.

  Returns a stateful transducer when no collection is provided.

  See also `dedupe-by`."
  ([f]
   (fn [rf]
     (let [seen (transient #{})]
       (fn
         ([] (rf))
         ([acc] (rf acc))
         ([acc x]
          (let [sig (f x)]
            (when-not (contains? seen sig)
              (conj! seen sig)
              (rf acc x))))))))
  ([f coll]
   (sequence (distinct-by f) coll)))

(defn ensure-ascending
  "Returns a stateful transducer that will ensure that only items that are ascending
  (using `compare`) will be emitted.

  Optionally takes an `f` which will be used to access the value.

  If called with a `coll` returns a lazy sequence."
  {:arglists '([]
               [f]
               [coll]
               [f coll])}
  ([] (ensure-ascending identity))
  ([f-or-coll]
   (if (coll? f-or-coll)
     (sequence (ensure-ascending identity) f-or-coll)
     (fn [xf]
       (let [last (volatile! nil)
             f    f-or-coll]
         (completing
           (fn [result item]
             (let [val (f item)]
               (if (neg? (compare @last val))
                 (do (vreset! last val)
                     (xf result item))
                 result))))))))
  ([f coll]
   (sequence (ensure-ascending f) coll)))

(defmacro assert-args
  "assert-args lifted from clojure.core. Mostly useful for writing other macros"
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))


(defmacro assert-info
  "A variant of `assert` that throws an `ex-info` based exception."
  [expr msg info]
  `(when (not ~expr)
     (throw (ex-info ~msg ~info))))

(defn map-keys
  "Returns a transducer for mapping `f` over all keys in a map-entry.

  If called with `map`, returns a new map with `f` applied over all keys."
  ([f] (core/map (fn [[k v]] [(f k) v])))
  ([f map] (when map (into (with-meta {} (meta map)) (map-keys f) map))))

(defn map-vals
  "Returns a transducer for mapping `f` over all values in a map-entry.

  If called with `map`, returns a new map with `f` applied over all values."
  ([f] (core/map (fn [[k v]] [k (f v)])))
  ([f map] (when map (into (with-meta {} (meta map)) (map-vals f) map))))

(defn map-leaves
  "Returns a transducer for mapping `f` over all leaf values in a map-entry. Nested maps will be
  traversed.

  If called with `map` returns a new map with `f` applied over all leaves."
  ([f] (core/map (fn [[k v]] [k (if (map? v)
                                  (map-leaves f v)
                                  (f v))])))
  ([f map] (when map (into (with-meta {} (meta map)) (map-leaves f) map))))

(defn remove-vals
  "Return a transducer which will only match map-entries for which the
  `pred` called on values returned logical `false`.

  If called with `map`, will return a new map executing the transducer."
  ([f] (remove (fn [[_ v]] (f v))))
  ([f map] (when map (into (with-meta {} (meta map)) (remove-vals f) map))))

(defn filter-vals
  "Return a transducer which will only match map-entries for which the
  `pred` called on values returned logical `true`.

  If called with `map`, will return a new map executing the transducer."
  ([f] (filter (fn [[_ v]] (f v))))
  ([f map] (when map (into (with-meta {} (meta map)) (filter-vals f) map))))

(defn remove-keys
  "Return a transducer which will only match map-entries for which the
  `pred` called on keys returned logical `false`.

  If called with `map`, will return a new map executing the transducer."
  ([f] (remove (fn [[k _]] (f k))))
  ([f map] (when map (into (with-meta {} (meta map)) (remove-keys f) map))))

(defn filter-keys
  "Return a transducer which will only match map-entries for which the
  `pred` called on keys returned logical `true`.

  If called with `map`, will return a new map executing the transducer."
  ([f] (filter (fn [[k _]] (f k))))
  ([f map] (when map (into (with-meta {} (meta map)) (filter-keys f) map))))

(defn ns-keys
  "Returns a transducer which will namespace all keys in a map.

  If called with `map` returns a new map executing the transducer."
  ([ns] (map-keys (comp (partial keyword ns) name)))
  ([ns map] (map-keys (comp (partial keyword ns) name) map)))

(defn assoc-some
  "Like `clojure.core/assoc` but only associates the key and value if the `val` is `some?`."
  [m & {:as kvs}]
  (reduce-kv
    assoc
    m
    (filter-vals some? kvs)))


(defn fex
  "Similar `clojure.core/fnil`. Returns a function which will call `f` when called
  and return `ex-val` in the case that any exception occurs."
  [f ex-val]
  (fn [& args]
    (try (apply f args)
         (catch #?(:clj Exception
                   :cljs js/Error) _
           ex-val))))

(defn find-paths
  "Returns a sequence of vector paths for which `pred` returns true.

  For nested maps, it will be called on the `pred` will be called with the `val` of the map entry.

  Note that this performs a pre-walk and in the case of nested maps or vectors, `pred` will be
  called with them before traversing their children."
  [pred coll]
  (let [results (transient [])]
    (w.walk/pathwalk-pre
      (fn [path form]
        (when (pred form)
          (conj! results path))
        form)
      coll)
    (persistent! results)))

(defn dissoc-in
  "Like `clojure.core/dissoc` but takes a path like `clojure.core/get-in`"
  [m path]
  (let [path-count (count path)]
    (if (= 1 path-count)
      (dissoc m (first path))
      (let [[update-tgt [dissoc-tgt]] (split-at (- path-count 1) path)]
        (update-in m update-tgt dissoc dissoc-tgt)))))

(defn index-by
  "Indexes a `coll` by the value returned by `f`. Returns a map.

  Optionally takes a function `g` which will be applied to the val in the map.

  user=> (index-by :name [{:name \"Bob\" :age 42}])
  {\"Bob\" {:name \"Bob\" :age 42}}

  user=> (index-by :name :age [{:name \"Bob\" :age 42}])
  {\"Bob\" 42}"
  ([f coll] (index-by f identity coll))
  ([f g coll] (into {} (map (juxt f g)) coll)))

(defn group-by
  "Similar to `clojure.core/group-by`. Returns a map with keys of
  `f` applied to items in `coll`.

  Optionally takes a function `g` which will be applied to the values.

  Lastly, takes an `into` collection, which will be used to create the base collection at each key.
  This allows you to specify grouping into sets or vectors."
  ([f coll] (group-by f identity [] coll))
  ([f g coll] (group-by f g [] coll))
  ([f g into coll]
   (reduce #(update %1 (f %2) (fnil conj into) (g %2))
           {}
           coll)))

(defn unfold
  "Produces a lazy sequence by invoking f with initial.

  `f` should return a tuple of an item and new state.

  If the item is `nil` nothing will be emitted.

  If `f` returns `nil` the sequence will terminate.

  Deprecated. Use either `clojure.core/iterate` or `clojure.core/iteration`"
  {:deprecated true}
  [f initial]
  (when-let [[val new-state] (f initial)]
    (lazy-seq
      (if val
        (cons val (unfold f new-state))
        (unfold f new-state)))))

(defn arity
  "Return a variant of `f` that will only accept `n` arguments.

  Useful for coercing polyvariadic functions into fixed arity ones.

  Optionally takes `&args` which will be applied after the first `n`
  arguments.

  For getting the arity of a function, see `fn-arity`"
  {:arglists '([n f & static-args])}
  [n f & static-args]
  (fn [& call-args]
    (let [args (concat (take n call-args)
                       static-args)]
      (apply f args))))

(defmacro make-map
  "Packs a sequence of variables into a map with keywords bound to the same name
  as the variables"
  [& args]
  (let [data (loop [[a & [b & b-rest :as a-rest] :as args] args
                    data                                   []]
               (cond
                 (empty? args) data
                 (symbol? a)   (recur
                                 a-rest
                                 (-> data
                                     (conj (keyword a))
                                     (conj a)))
                 :else         (recur
                                 b-rest
                                 (-> data
                                     (conj a)
                                     (conj b)))))]
    `(array-map ~@data)))

(defn compr
  "Composes the provided reducer functions `fs`.

  This essentially behaves exactly as `comp` but is variadic where additional arguments will be
  passed to each function."
  [& fs]
  (fn [acc & args]
    (loop [[f & _ :as fs] fs
           acc            acc]
      (if-not (seq fs)
        acc
        (recur (rest fs) (apply f acc args))))))

(defn over
  "Returns a function which is `g` mapped over its variadic input and applied to `f`"
  [f g]
  (fn [& args]
    (apply f (map g args))))

(defn toggle
  "Return a set of items `s` with the presence of `val` toggled."
  [s val]
  ((if (contains? s val)
     disj
     conj) s val))

#?(:clj
   (defn fn-arities
     "Return a set of all arities defined for `f`.

  A variadic function will return a set of `#{0 1}`"
     [f]
     (->> f
          class
          (.getDeclaredMethods)
          (map #(-> % .getParameterTypes alength))
          (set))))

#?(:clj
   (defn fn-arity
     "Function to get the arity of the provided function `f`

  For functions that have multiple arities defined, returns the highest defined
  arity.

  For the function combinator see `arity`"
     [f]
     (apply max (fn-arities f))))

(comment
  (toggle #{:a :b} :c)
  (toggle #{:a :b} :b)

  (update {:k #{:a :b}} :k toggle :c)
  (update {:k #{:a :b}} :k toggle :b)
  )
