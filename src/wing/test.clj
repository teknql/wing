(ns wing.test
  "Namespace providing testing facilities"
  (:require [clojure.core.match :refer [match]]
            [slingshot.slingshot :refer [try+]]
            [clojure.test :refer [do-report]]))


(defmacro assert-match
  "Macro which asserts that `result` matches the `match-pattern`, throwing an
  exception if it does not."
  [result match-pattern]
  `(let [result# ~result]
     (match result#
       ~match-pattern (do-report {:expected '~match-pattern
                                  :actual   result#
                                  :message  "found successful match"
                                  :type     :pass})
       :else (do-report {:expected '~match-pattern
                         :actual   result#
                         :message  "could not find a match"
                         :type     :fail}))))

(defmacro assert-exception
  "Macro for asserting on the shape of the data provided by `ex-info`.
  See `slingshot.slingshot/try+` for more details on the bindings.

  An example that would match:

  (assert-exception [:type :foo] (throw (ex-info \"Error\" {:type :foo})))"
  [slingshot-bindings & body]
  (let [&throw-context '&throw-context]
    `(try+
      (let [result# (do ~@body)]
        (do-report {:expected '~slingshot-bindings
                    :actual   result#
                    :message  "No exception was thrown."
                    :type     :fail}))
      (catch ~slingshot-bindings e#
        (do-report {:expected '~slingshot-bindings
                    :actual   (:throwable ~&throw-context)
                    :message  "expected exception found"
                    :type     :pass}))
      (catch Object e#
        (do-report {:expected '~slingshot-bindings
                    :actual   (:throwable ~&throw-context)
                    :message  "Exception did not match expected pattern"
                    :type     :fail})))))
