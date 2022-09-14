(ns wing.malli
  "Common schema and tools for malli"
  (:require [cuerdas.core :as str]
            [wing.malli.json :as wm.json]
            [malli.transform :as mt]
            [malli.core :as m]))

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
    (reduce conj [:enum {:error/message  error
                         :enum/namespace val-ns}] vals)))

(defn json-transformer
  "JSON transformer which will auotmatically encode / decode namespaced keywords into flatter JSON"
  []
  (wm.json/transformer))


(defn default-fn-transformer
  "Malli transformer that will invoke `:default-fn` on maps with missing entries.

  Useful for setting default query parameters"
  []
  (mt/transformer
    {:name     :default-fn
     :decoders {:map
                {:compile
                 (fn [schema _]
                   (let [default-fns (->> (m/children schema)
                                          (keep (fn [[k {:keys [default-fn]} _]]
                                                  (when default-fn
                                                    [k default-fn])))
                                          (into {}))]
                     (fn [x]
                       (if (map? x)
                         (reduce-kv
                           (fn [acc k default]
                             (if-not (contains? x k)
                               (assoc acc k (default))
                               acc))
                           x
                           default-fns)
                         x))))}}}))

(defn empty-string-as-nil-transformer
  "Malli transformer that will coerce empty strings into `nil` on decode."
  []
  (let [empty-string->nil #(if (and (string? %) (empty? %))
                             nil
                             %)]
    (mt/transformer
      {:name     :empty-string-as-nil
       :decoders {'string? empty-string->nil
                  :string  empty-string->nil}})))

(defn strip-nil-keys-transformer
  "Mall transformer that will remove `nil` value keys from a map on decode."
  []
  (mt/transformer
    {:name     :strip-nil-keys
     :decoders {:map {:leave #(if (map? %)
                                (reduce-kv
                                  (fn [acc k v]
                                    (if (some? v)
                                      (assoc acc k v)
                                      acc))
                                  {}
                                  %)
                                %)}}}))
