(ns wing.core.time
  "Utilities for working with time"
  (:require [tick.core :as t]
            [wing.core :as w]))

(defn divisible?
  "Returns whether duration `a` is evenly divisible by `b`. "
  [a b]
  (int? (t/divide a b)))

(defn round-to
  "Returns `time` rounded down to an evenly divisible multiple of `duration`.

  Automatically truncates to milliseconds."
  [time duration]
  (let [time     (t/truncate time :millis)
        t-millis (t/millis (t/between (t/epoch) (t/instant time)))
        d-millis (t/millis duration)
        diff     (mod t-millis d-millis)]
    (-> time
        (t/<< (t/new-duration diff :millis)))))


(defn ensure-chronological
  "Return a stateful transducer which will ensure that items that items are emitted in
  chronological order. Items that happen after the latest will be filtered.

  Optionally takes a `f` that will be used to access the time field. Usually a keyword if specified.

  If called with a collection, will return a sequence of the transducer applied to the collection."
  {:arglists '([]
               [f]
               [coll]
               [f coll])}
  [& args]
  (apply w/ensure-ascending args))
