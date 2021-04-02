(ns dev
  (:require
    [datomic.api :as d]
    [yada.yada :as yada]
    [juxt.clip.core :as h]
    [juxt.clip.repl :as repl]))

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
    :handler {:start `(find-seed-resource (clip/ref :db))}
    :http {:start `(yada/listener (clip/ref :handler))
           :stop '((:close this))
           :resolve :server}
    :foo {:start '(clip/ref :http)}}
   :reloads
   `{yada.resource.Resource juxt.clip.repl.yada/reloadable-resource}})

(comment
  (println (str "http://localhost:" (:port (:http repl/system))))

  (repl/set-init! #(deref #'system-config))
  (repl/start)
  )
