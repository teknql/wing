(ns wing.repl
  "REPL namespace for Wing"
  (:require [clojure.tools.deps.alpha :as tools.deps]
            [cemerick.pomegranate :as pome]))

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
