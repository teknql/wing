(ns wing.core.resource
  "Namespace for working with resources in Clojure"
  (:require [wing.core :as w]))


(defprotocol Resource
  (release [r]
    "Releases the provided resource. Called automatically via `with-resource`"))

(extend-protocol Resource
  java.io.File
  (release [file] (.close file))
  Object
  (release [_] nil))

(defmacro with-resource
  "bindings => [name init ...]

  Evaluates body in a try expression with names bound to the values of the
  inits, and a finally clause that calls `release` on each name in reverse
  order."
  [bindings & body]
  (w/assert-args
    (vector? bindings) "a vector for its binding"
    (even? (count bindings)) "an even number of forms in binding vector")
  (cond
    (empty? bindings)      `(do ~@body)
    (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                              (try
                                (with-resource ~(subvec bindings 2) ~@body)
                                (finally (release ~(bindings 0)))))
    :else
    (throw
      (IllegalArgumentException.
        "with-resource only allows Symbols in bindings"))))
