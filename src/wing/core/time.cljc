(ns wing.core.time
  "Utilities for working with time"
  #?(:clj
     (:require [java-time :as time])
     :cljs
     (:require [cljs-time.core :as time])))

(defn- ->millis
  "Clojure / Clojurescript agnostic way of getting a time in milliseconds"
  [t]
  #?(:clj (time/as t :millis)
     :cljs (time/in-millis t)))

(defn divide
  "Returns the result of dividing duration `a` by `b`."
  [a b]
  (/ (->millis a)
     (->millis b)))


(defn divisible?
  "Returns whether duration `a` is evenly divisible by `b`.

  Does not support units smaller than `:millis`"
  [a b]
  (let [a-millis (->millis a)
        b-millis (->millis b)]
    (= 0 (mod a-millis b-millis))))


#?(:clj
   (defn round-to
     "Returns `time` rounded down to an evenly divisible multiple of `duration`.

  Automatically truncates to milliseconds."
     [time duration]
     (let [t-millis (time/to-millis-from-epoch time)
           d-millis (time/as duration :millis)
           diff     (mod t-millis d-millis)]
       (-> time
           (time/minus (time/millis diff))
           (time/truncate-to :millis)))))


(defn on-or-after?
  "Returns whether `a` is equal to or after `b`.

  An inclusive version of `java-time/after?`."
  [a b]
  (or (= a b)
      (time/after? a b)))

(defn on-or-before?
  "Returns whether `a` is equal to or before `b`.

  An inclusive version of `java-time/before?`."
  [a b]
  (or (= a b)
      (time/before? a b)))
