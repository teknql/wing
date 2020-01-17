(ns wing.malli
  "Common schema and tools for malli"
  (:require [cuerdas.core :as str]
            [wing.malli.json-transformer :as wm.json]))

(defn enum-keyword
  "Creates an `:enum` schema which enforces that all of the keywords must be of the same
  namespace.

  When encoded to JSON, they will be de-namespaced and encoded as strings.

  => (m/encode (wm/enum-keyword [:color/red :color/green]) :color/green (wm/json-transformer))
  ;; \"green\""
  [vals]
  (let [val-labels (map  #(str/surround (name %) "'") vals)
        val-ns     (loop [ns         nil
                          [v & rest] vals]
                     (when ns
                       (assert (= ns (namespace v)) "Namespaces don't match"))
                     (if-not (seq rest)
                       (namespace v)
                       (recur (namespace v) rest)))
        error      (str "Must be one of: ["
                        (str/join "," val-labels)
                        "]")]
    (reduce conj [:enum {:error/message error
                         :encode/json   {:enter
                                         (fn [x]
                                           (if (keyword? x)
                                             (name x)
                                             x))}
                         :decode/json   {:enter
                                         (fn [x]
                                           (cond
                                             (string? x)  (keyword val-ns x)
                                             (keyword? x) (keyword val-ns (name x))
                                             :else        x))}}]
            vals)))

(defn json-transformer
  "JSON transformer which will auotmatically encode / decode namespaced keywords into flatter JSON"
  []
  (wm.json/transformer))
