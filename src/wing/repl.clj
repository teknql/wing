(ns wing.repl
  "REPL namespace for Wing"
  (:require [clojure.tools.deps.alpha :as tools.deps]
            [cemerick.pomegranate :as pome]
            [wing.integrant :as wing.ig]
            [integrant.core :as ig]
            [integrant.repl :as ig.repl]
            [integrant.repl.state]))

(defn- deps->class-paths!
  "Convert a map of deps into a sequence of class paths.

  See `clojure.tools.deps.alpha/resolve-deps` for `arg-maps` documentation."
  ([deps] (deps->class-paths! deps {}))
  ([deps arg-map]
   (->> (tools.deps/resolve-deps
          {:deps deps
           :mvn/repos
           {"central" {:url "https://repo1.maven.org/maven2/"}
            "clojars" {:url "https://repo.clojars.org/"}}}
          arg-map)
        (vals)
        (mapcat :paths))))

(defn sync-libs!
  "Dynamically refreshes all libs in `deps.edn` and installs any missing.

  Optionally takes an alias to look under."
  ([] (sync-libs! nil))
  ([?alias]
   (let [edn        (-> "deps.edn" slurp read-string)
         deps       (get edn :deps)
         extra-deps (when ?alias
                      (get-in edn [:aliases ?alias :extra-deps]))]
     (->> (deps->class-paths! deps {:extra-deps extra-deps})
          (run! pome/add-classpath)))))


(defn add-lib!
  "Add a single library to the active classpath."
  [lib-sym lib-spec]
  (->> (deps->class-paths! {lib-sym lib-spec})
       (run! pome/add-classpath)))


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
