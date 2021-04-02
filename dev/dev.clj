(ns dev
  (:require
    [datomic.api :as d]
    [yada.yada :as yada]
    [juxt.clip.core :as h]
    [juxt.clip.repl.yada :as juxt.clip.repl.yada]))

(defn seed-conn
  [conn]
  @(d/transact conn [{:db/ident :test
                      :db/doc "aha, foo."}]))

(defn find-seed-resource
  [conn]
  (yada/resource
    {:methods
     {:get
      {:produces ["text/html"]
       :response
       (fn [ctx]
         (d/pull
           (d/db conn)
           '[*]
           :test))}}}))

(defn ring-handler
  [db]
  (fn [req]
    {:status 200
     :body "Hello?  How are you"}))

(def system-config
  {:components
   {:db {:pre-start `(d/create-database "datomic:mem://newdb")
         :start `(d/connect "datomic:mem://newdb")
         :post-start `seed-conn}
    ;; :handler {:start `(find-seed-resource (clip/ref :db))}
    :handler {:start `(ring-handler (clip/ref :db))}
    :http {:start `(yada/listener (clip/ref :handler))
           :stop '((:close this))
           :resolve :server}
    :foo {:start '(clip/ref :http)}}
   :reloads {yada.resource.Resource juxt.clip.repl.yada/reloadable-resource
             clojure.lang.Fn juxt.clip.repl/relodable-fn}})

(def rf-system
  (#'h/exec-queue
    (for [f [(#'h/pre-starting-f system-config)
             (#'h/starting-f system-config)
             (#'h/post-starting-f system-config)]
          component (#'h/component-chain system-config)]
      (f component))))

(comment
  (#'h/run
    (comp #'h/stopping)
    system
    system-config
    (#'h/reverse-component-chain system-config))

  (#'h/exec-queue
    (map #'h/stopping-f (#'h/reverse-component-chain system-config))
    rf-system)

  (throw (ex-info "foo" {}))

  (def e *e)

  (ex-data e)

  (ex-data (ex-cause e))

  (:port (:http juxt.clip.repl/system))

  (juxt.clip.repl/set-init! #(deref #'system-config))
  (juxt.clip.repl/go)
  (juxt.clip.repl/stop)
  (juxt.clip.repl/reset)
  )
