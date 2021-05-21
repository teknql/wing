(ns wing.log
  "Extensions to timbre's logging facilities."
  (:require [taoensso.timbre :as log]
            [puget.printer :as puget]
            [puget.color.ansi :as ansi]
            [raven-clj.core :as raven]
            [raven-clj.interfaces :as interfaces]))

(def ^:private ^:const system-newline
  (System/getProperty "line.separator"))

(defn- atomic-println
  "Atomically print `x`."
  [x]
  (print (str x system-newline)) (flush))

(def ^:private level->color
  {:trace  :white
   :debug  :white
   :info   :blue
   :warn   :yellow
   :error  :red
   :fatal  :red
   :report :red})

(defn pretty-println-appender
  "Create an appender that looks delightful"
  ([] (pretty-println-appender {}))
  ([{:keys [stream]
     :or   {stream :auto}}]
   {:enabled?   true
    :async?     false
    :min-level  nil
    :rate-limit nil
    :output-fn  (fn [{:keys [timestamp_ vargs level ?line ?ns-str ?err context]}]
                  (let [[msg data] vargs
                        header     (format "%s %s [%s:%d] - %s"
                                           (ansi/sgr (name level) (level->color level))
                                           (force timestamp_)
                                           ?ns-str
                                           ?line
                                           msg)]
                    (str header
                         (when (or data context)
                           (str " "
                                (puget/with-color
                                  (puget/pprint-str (merge data context)))))
                         (when ?err
                           (str "\n"
                                (log/stacktrace ?err))))))
    :fn
    (fn [{:keys [output_] :as data}]
      (let [stream (case stream
                     :auto  (if (:error-level? data) *err* *out*)
                     :*out* *out*
                     :*err* *err*)]
        (binding [*out* stream]
          (atomic-println (force output_)))))}))

(def ^:private timbre->sentry-levels
  {:trace  "debug"
   :debug  "debug"
   :info   "info"
   :warn   "warning"
   :error  "error"
   :fatal  "fatal"
   :report "info"})

(defn sentry-appender
  "Returns a raven-clj Sentry appender.
  Requires the DSN (e.g. \"https://<key>:<secret>@sentry.io/<project>\")
  to be passed in, see Sentry documentation for details.
  Common options:
    * :tags, :environment, :release, and :modules will be passed to Sentry
      as attributes, Ref. https://docs.sentry.io/clientdev/attributes/.
    * :event-fn can be used to modify the raw event before sending it
      to Sentry."

  [dsn & [opts]]
  (let [{:keys [event-fn] :or {event-fn identity}} opts
        base-event
        (->> (select-keys opts [:tags :environment :release :modules])
             (filter (comp not nil? second))
             (into {}))]

    {:enabled?   true
     :async?     true
     :min-level  :warn ; Reasonable default given how Sentry works
     :rate-limit nil
     :output-fn  :inherit
     :fn
     (fn [data]
       (let [{:keys [level ?err ?ns-str context vargs]} data
             [msg data]                                 vargs
             event
             (as-> base-event event
               (merge event
                      {:message msg
                       :logger  ?ns-str
                       :level   (get timbre->sentry-levels level)}
                      (when (or context data)
                        {:extra (merge data context)}))
               (if ?err
                 (interfaces/stacktrace event ?err)
                 event)

               (event-fn event))]

         (raven/capture dsn event)))}))
