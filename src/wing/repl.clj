(ns wing.repl
  "REPL namespace for Wing"
  (:require [clojure.tools.deps.alpha.repl :refer [add-lib]]
            [wing.integrant :as wing.ig]
            [integrant.core :as ig]
            [integrant.repl :as ig.repl]
            [integrant.repl.state]))

(defn sync-libs
  "Dynamically refreshes all libs in `deps.edn` and installs any missing.

  Optionally takes an alias to look under."
  ([] (sync-libs nil))
  ([?alias]
   (let [edn  (-> "deps.edn" slurp read-string)
         libs (if ?alias
                (get-in edn [:aliases ?alias :extra-deps])
                (get edn :deps))]
     (doseq [[lib-sym lib-spec] libs]
       (add-lib lib-sym lib-spec)))))

(defn find-config
  "Returns a running integrant config with the provided key.
  If multiple components can be found, returns a list of them"
  [k]
  (if-not integrant.repl.state/config
    :not-running
    (let [items (ig/find-derived integrant.repl.state/config k)]
      (case (count items)
        0     :not-found
        1     (-> items first second)
        :else items))))

(defn find-running
  "Returns a running integrant component with the provided key.
  If multiple components can be found, returns a list of them"
  [k]
  (if-not integrant.repl.state/system
    :not-running
    (let [items (ig/find-derived integrant.repl.state/system k)]
      (case (count items)
        0     :not-found
        1     (-> items first second)
        :else items))))

(defn bootstrap-integrant!
  "Bootstraps integrant by calling `integrant.repl/set-prep!`"
  []
  (ig.repl/set-prep! wing.ig/load-config))

(defn bootstrap!
  "Bootstraps Wingman. Likely called from your user.clj"
  []
  (bootstrap-integrant!))
