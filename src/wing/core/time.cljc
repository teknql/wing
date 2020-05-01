(ns wing.core.time
  "Utilities for working with time"
  (:require [tick.alpha.api :as t]))

(defn divisible?
  "Returns whether duration `a` is evenly divisible by `b`. "
  [a b]
  (int? (t/divide a b)))

(defn round-to
  "Returns `time` rounded down to an evenly divisible multiple of `duration`.

  Automatically truncates to milliseconds."
  [time duration]
  (let [t-millis (t/millis (t/between (t/epoch) (t/instant time)))
        d-millis (t/millis duration)
        diff     (mod t-millis d-millis)]
    (-> time
        (t/- (t/new-duration diff :millis))
        (t/truncate :millis))))
