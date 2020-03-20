(ns wing.malli.json-transformer
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
  (let [allowed-keys (map first (m/-map-entries schema))
        props        (m/-properties schema)
        root-ns      (or (:json/root-namespace props)
                         (->> (frequencies (map namespace allowed-keys))
                              (sort-by val)
                              (last)
                              key))
        rename-map   (into {}
                           (map
                             (fn [k]
                               [k (let [ns   (namespace k)
                                        name (name k)]
                                    (if (= root-ns ns)
                                      (encode-map-key name)
                                      (encode-map-key
                                        (str/replace
                                          (str ns "." name) (re-pattern (str "^" root-ns "\\.")) ""))))]))
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

(defn transformer
  "JSON transformer which will auotmatically encode / decode namespaced keywords into flatter JSON

  Responds to the malli transformer option of: `:json/encode-map-key` for building encoders
  and decoders. Mostly useful to pair with `cuerdas.core` string functions. By default will
  use snake case."
  []
  (mt/transformer
    {:name :json
     :encoders
     {:map        {:compile encode-namespaced-keys}
      :sequential {:leave vec}}
     :decoders
     {:map        {:compile decode-namespaced-keys}
      'keyword?   {:enter #(apply keyword (str/split % "/"))}
      :sequential {:leave vec}}}))
