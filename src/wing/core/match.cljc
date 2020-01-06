(ns wing.core.match
  "Extensions to `core.match`"
  (:require [clojure.core.match :refer [match]]))

(defmacro match?
  "Returns whether the provided `expr` matches the given `match-pattern`."
  [match-pattern expr]
  `(let [val# ~expr]
     (match val#
       ~match-pattern true
       :else false)))
