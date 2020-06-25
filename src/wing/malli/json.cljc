(ns wing.malli.json
  "Custom JSON transformer which auto-encodes namespaced map keys"
  (:require [malli.core :as m]
            [cuerdas.core :as str]
            [clojure.set :as set]
            [malli.transform :as mt]))

(defn generate-encode-key-rename-map
  "Function to generate a map of key renames for a provided
  schema with the given options."
  [schema {encode-map-key :json/encode-map-key
           :or            {encode-map-key str/snake}}]
  (let [map-entries      (m/-map-entries schema)
        allowed-keys     (map first map-entries)
        explicit-renames (->> map-entries
                              (keep #(when-some [explicit (-> % second :json/key)]
                                       [(first %) explicit]))
                              (into {}))
        props            (m/-properties schema)
        root-ns          (or (:json/root-namespace props)
                             (->> (frequencies (map namespace allowed-keys))
                                  (sort-by val)
                                  (last)
                                  key))
        rename-map       (into {}
                               (map
                                 (fn [k]
                                   [k (or (get explicit-renames k)
                                          (let [ns   (namespace k)
                                                name (name k)]
                                            (if (= root-ns ns)
                                              (encode-map-key name)
                                              (encode-map-key
                                                (str/replace
                                                  (str ns "." name) (re-pattern (str "^" root-ns "\\.")) "")))))]))
                               allowed-keys)]
    rename-map))


(defn encode-namespaced-keys
  "Returns an interceptor that will encode keys as strings as well as filtering
  the map to only allowed keys."
  [schema opts]
  (let [rename-map   (generate-encode-key-rename-map schema opts)
        allowed-keys (keys rename-map)]
    {:leave
     (fn [x]
       (-> x
           (select-keys allowed-keys)
           (set/rename-keys rename-map)))}))

(defn decode-namespaced-keys
  "Returns an interceptor that will decode string keys on enter."
  [schema opts]
  (let [rename-map   (set/map-invert (generate-encode-key-rename-map schema opts))
        allowed-keys (keys rename-map)]
    {:enter
     (fn [x]
       (-> x
           (select-keys allowed-keys)
           (set/rename-keys rename-map)))}))

(defn encode-enum-keywords
  "Return an interceptor that will encode enum namespaced keywords"
  [schema opts]
  (when (-> schema m/-properties :enum/namespace)
    (let [encode (:json/encode-enum opts str/snake)]
      {:enter
       (fn [x]
         (if (keyword? x)
           (encode (name x))
           x))})))

(defn decode-enum-keywords
  "Return an interceptor that will encode enum namespaced keywords"
  [schema _opts]
  (when-some [val-ns (-> schema m/-properties :enum/namespace)]
    {:leave
     (fn [x]
       (cond
         (string? x)  (keyword val-ns (str/kebab x))
         (keyword? x) (keyword val-ns (str/kebab (name x)))
         :else        x))}))

(defn transformer
  "JSON transformer which will auotmatically encode / decode namespaced keywords into flatter JSON

  Responds to the malli transformer option of: `:json/encode-map-key` for building encoders
  and decoders, as well as `:json/encode-enum` for enum keywords.

  Mostly useful to pair with `cuerdas.core` string functions. By default will use snake case."
  []
  (mt/transformer
    {:name :json
     :encoders
     {:map        {:compile encode-namespaced-keys}
      :sequential {:leave vec}
      :enum       {:compile encode-enum-keywords}}
     :decoders
     {:map        {:compile decode-namespaced-keys}
      'keyword?   {:enter #(apply keyword (str/split % "/"))}
      :sequential {:leave vec}
      :enum       {:compile decode-enum-keywords}}}))
