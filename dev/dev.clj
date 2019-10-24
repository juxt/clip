(ns dev
  (:require
    [datomic.api :as d]
    [yada.yada :as yada]
    [io.dominic.high.core :as h]))

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

(def system-config
  {:components
   {:db {:pre-start `(d/create-database "datomic:mem://newdb")
         :start `(d/connect "datomic:mem://newdb")
         :post-start `seed-conn}
    :handler {:start `(find-seed-resource (high/ref :db))}
    :http {:start `(yada/listener (high/ref :handler))
           :stop '((:close this))
           :resolve :server}
    :foo {:start '(high/ref :http)}}})

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
  )
