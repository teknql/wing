(ns wing.uix.router
  (:require [uix.core.alpha :refer [defcontext] :as uix]
            [reitit.frontend :as rf]
            [reitit.frontend.history :as rfh]
            [reitit.coercion.malli :as r.malli]
            [malli.core :as m]
            [malli.transform :as mt]
            [wing.malli :as wm]))


(defcontext ^{:doc "Context for the active reitit router"} *router*)
(defcontext ^{:doc "Context for the active reitit match"} *match*)
(defcontext ^{:doc "Context for the active reitit front-end history"} *history*)

(defn router-provider
  "Build a reitit powered router-provider using the provided routes and malli registry extensions."
  [{:keys [routes registry-extensions]} comp]
  (let [router     (rf/router
                     routes
                     {:data {:coercion
                             (-> r.malli/default-options
                                 (assoc-in [:options :registry]
                                           (merge (m/default-schemas)
                                                  registry-extensions))
                                 (assoc-in [:transformers :string :default]
                                           (mt/transformer
                                             (mt/string-transformer)
                                             (wm/default-fn-transformer)
                                             (wm/empty-string-as-nil-transformer)
                                             (wm/strip-nil-keys-transformer)))
                                 (r.malli/create))}})
        history    (uix/state nil)
        match      (uix/state nil)]
    (uix/with-effect []
      (let [history-val (rfh/start! router #(reset! match %) {:use-fragment false})]
        (reset! history history-val)
        #(do (rfh/stop! history-val)
             (reset! history nil))))
    (uix/context-provider
      [*router* router]
      (uix/context-provider
        [*match* @match]
        (uix/context-provider [*history* @history]
                              [comp])))))

(defn use-match
  "Return the value of the active match"
  []
  (uix/context *match*))

(defn use-data
  "Return the route data from the active match"
  []
  (:data (use-match)))

(defn use-route-parameters
  "Return a state atom with `:params` and `:query` as set via the route.

  Editing the state atom will result in the route being replaced.

  Optionally takes a path which will be used to return a cursor into the state."
  ([]
   (let [match      (use-match)
         hist       (uix/context *history*)
         state      (uix/state (:parameters match))
         last-state (uix/ref (:parameters match))]
     (uix/effect!
       #(reset! state (:parameters match))
       [match])

     (uix/effect!
       #(do (when-not (= @last-state @state)
              (rfh/replace-state hist
                                 (-> match :data :name)
                                 (:path @state)
                                 (:query @state)))
            (reset! last-state @state))
       [@state])
     state))
  ([cursor-path]
   (uix/cursor-in (use-route-parameters) cursor-path)))

(defn href
  "Return a URL string for the provided `page-name`"
  ([page-name] (href page-name nil nil))
  ([page-name params] (href page-name params nil))
  ([page-name params query]
   (let [history (uix/context *history*)]
     (when history
       (rfh/href history page-name params query)))))
