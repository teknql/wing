(ns wing.core.match
  "Extensions to `core.match`"
  #?(:clj (:require [clojure.core.match :as clj]
                    [cljs.core.match :as cljs])
     :cljs (:require [cljs.core.match :as cljs])))

(defmacro match?
  "Returns whether the provided `expr` matches the given `match-pattern`."
  [match-pattern expr]
  (let [cljs? (some? (:js-globals &env))]
    (if-not cljs?
      `(let [val# ~expr]
         (clj/match val#
           ~match-pattern true
           :else false))
      `(let [val# ~expr]
         (cljs/match val#
           ~match-pattern true
           :else false)))))
