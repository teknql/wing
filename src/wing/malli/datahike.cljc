(ns wing.malli.datahike
  "Namespace for interoping malli and datahike"
  (:require [malli.core :as m]))


(defn- collection-like?
  "Returns whether the schema is collection-like"
  [schema]
  (#{:sequential :vector :list :set} (m/type schema)))

(defn- map-like?
  "Returns whether the schema is map-like"
  [schema]
  (#{:map} (m/type schema)))

(defn- ns-select-keys
  "Selects all keys from the provided `map` that match the given `ns-str`"
  [ns-str map]
  (into {} (filter (comp #{ns-str} namespace key)) map))

(defn ->value-type
  "Returns the value type for the schema"
  [schema]
  (or (:db/valueType (m/properties schema))
      ({'string?         :db.type/string
        'int?            :db.type/long
        'pos-int?        :db.type/long
        'nat-int?        :db.type/long
        'inst?           :db.type/instant
        'decimal?        :db.type/bigdec
        'boolean?        :db.type/boolean
        'keyword?        :db.type/keyword
        'simple-keyword? :db.type/keyword
        'symbol?         :db.type/symbol
        'uuid?           :db.type/uuid
        'number?         :db.type/float
        'float?          :db.type/float
        'double?         :db.type/double
        :map             :db.type/ref} (m/type schema))))

(defn ->root-value-type
  "Traverses collection-like schema types to return their underlying
  value type"
  [schema]
  (if (collection-like? schema)
    (->root-value-type (first (m/children schema)))
    (->value-type schema)))

(defn ->datahike-schema
  "Function to convert a malli schema into a datahike schema"
  ([schema] (->datahike-schema schema nil))
  ([schema opts]
   (->> (m/children schema opts)
        (map
          (fn [[child-k map-child-props child-schema]]
            (let [all-child-props (merge (m/properties child-schema)
                                         map-child-props)]
              (cons
                (merge {:db/ident       child-k
                        :db/valueType   (->root-value-type child-schema)
                        :db/cardinality (if (collection-like? child-schema)
                                          :db.cardinality/many
                                          :db.cardinality/one)}
                       (ns-select-keys "db" all-child-props))
                (when (map-like? child-schema)
                  (->datahike-schema child-schema opts))))))
        (flatten))))
