(ns wing.integrant
  "Integrant specific functionality for Wingman"
  (:require [duct.core :as duct]
            [integrant.core :as ig]
            [clojure.java.io :as io]))

(defn load-config
  "Loads the integrant config for integrant"
  []
  (duct/load-hierarchy)
  (-> (io/resource "config.edn")
      (duct/read-config)
      (doto ig/load-namespaces)))

;; Once pluggable spec lands
#_(defmethod ig/assert-pre-init-spec :default [system key value]
    (when-some [schema (ig/pre-init-spec key)]
      (when-some [error (m/explain schema value)]
        (throw (ex-info (str "Schema failed on key " key "when building system\n")
                        {:reason  ::ig/build-failed-spec
                         :system  system
                         :key     key
                         :value   value
                         :schema  schema
                         :explain error})))))
