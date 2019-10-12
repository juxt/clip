(ns io.dominic.high.core
  (:require
    [clojure.set :as set]))

(defn- sccs
  "Returns a topologically sorted list of strongly connected components.
  Tarjan's algorithm."
  [g]
  (let [strong-connect (fn strong-connect
                         [acc v]
                         (let [acc (-> acc
                                       (assoc-in [:idxs v] (:idx acc))
                                       (assoc-in [:low-links v] (:idx acc))
                                       (update :idx inc)
                                       (update :S conj v)
                                       (assoc-in [:on-stack v] true))
                               acc (reduce (fn [acc w]
                                             (cond
                                               (not (get-in acc [:idxs w]))
                                               (let [acc (strong-connect acc w)]
                                                 (update-in acc [:low-links v] min (get-in acc [:low-links w])))

                                               (get-in acc [:on-stack w])
                                               (update-in acc [:low-links v] min (get-in acc [:idxs w]))

                                               :else
                                               acc))
                                           acc
                                           (get g v))]
                           (if (= (get-in acc [:idxs v]) (get-in acc [:low-links v]))
                             (let [[S on-stack scc] (loop [S (:S acc)
                                                           on-stack (:on-stack acc)
                                                           scc #{}]
                                                      (let [w (peek S)
                                                            S (pop S)
                                                            on-stack (dissoc on-stack w)
                                                            scc (conj scc w)]
                                                        (if (= v w)
                                                          [S on-stack scc]
                                                          (recur S on-stack scc))))]
                               (-> acc
                                   (assoc :S S :on-stack on-stack)
                                   (update :sccs conj scc)))
                             acc)))]
    (:sccs
      (reduce
        (fn [acc v]
          (if-not (contains? (:idxs acc) v)
            (strong-connect acc v)
            acc))
        {:S ()
         :idx 0
         :sccs []}
        (keys g)))))

(defn- cycles
  [sccs g]
  (filter #(or (>= (count %) 2)
               (get-in g [(first %) (first %)]))
          sccs))

(defn- system-dependency-graph
  [system]
  (let [ref? #(= 'high/ref (and (coll? %) (first %)))
        ref-to second]
    (into {}
          (map (fn [[k v]]
                 [k (set (map ref-to (filter ref? (tree-seq coll? seq (:start v)))))])
               system))))

(defn- dependency-errors
  [sccs g]
  (concat
    (mapcat
      (fn [[k v]]
        (seq
          (map (fn [does-not-exist]
                 {:type :missing
                  :from k
                  :to does-not-exist})
               (remove #(contains? g %) v))))
      g)
    (map (fn [cycle] {:type :cycle :between cycle})
         (cycles sccs g))))

(comment
  (def system
    '{:db {:start (hikari-cp.core/make-datasource
                   {:jdbc-url
                    "jdbc:neo4j:bolt://host:port/?username=neo4j,password=xxxx,debug=true"})
          ;; No need to specify stop, implements java.io.Closeable
          }

     :db2 {:start
           (crux.api/start-standalone-node
             {:kv-backend "crux.kv.rocksdb.RocksKv"
              :event-log-dir "data/eventlog-1"
              :db-dir "data/db-dir-1"
              :backup-dir "checkpoint"})
           ;; No need to specify stop, implements java.io.Closeable
           }
     :yada {:start (yada.yada/listener
                     (high/ref :routes)
                     {:port 3000})

            :stop ((:close this))}

     :routes {:start
              (myapp.routes/routes
                (high/ref :db)
                (high/ref :db2)
                (high/ref :does-not-exist))
              ;; :stop not required, has no stop lifecycle
              }
     })

  (let [g (system-dependency-graph system)]
    (dependency-errors (sccs g) g))

  (let [g (system-dependency-graph system)]
    (cycles (sccs g) g))
  )

(defn- ref?
  [x]
  (= 'high/ref (and (coll? x) (first x))))

(def ^:private ref-to second)

(defn- resolve-refs
  [x ref-lookup]
  (clojure.walk/postwalk
    (fn [x]
      (if (ref? x)
        (get ref-lookup (ref-to x))
        x))
    x))

(defn- starting
  [running-system component]
  (let [resolved-start (resolve-refs (get component :start) running-system)]
    (try
      (eval resolved-start)
      (catch Throwable e
        (throw (ex-info
                 "Error while creating system"
                 {:partial-system running-system}
                 e))))))

;; rfish because it's kind-of a rf, but it returns a new element rather than
;; performing an action to reduce into the existing accumulator.  It may be
;; worth exploring whether a transducer-style next-rf could be used.  This
;; could have the upside of making rfish composable in the same way xforms are.
(defn- run
  [rfish component-chain]
  (reduce
    (fn [acc [id component]]
      (assoc acc id (rfish acc component)))
    {}
    component-chain))

(defn- promesa-run
  [rfish component-chain]
  (let [promise (requiring-resolve 'promesa.core/promise)
        then (requiring-resolve 'promesa.core/then)]
    (reduce
      (fn [prom-chain [id component]]
        (-> prom-chain
            (then (fn [acc]
                    (-> (rfish acc component)
                        (then (fn [res] (assoc acc id res))))))))
      (promise {})
      component-chain)))

(comment
  (def promesa-system
    '{:a {:start (promesa.core/resolved 5)}
      :b {:start (promesa.core/resolved (inc (high/ref :a)))}
      :c {:start (+ (high/ref :a) (high/ref :b))}})

  @(promesa-run
     starting
     (map #(find promesa-system (first %))
          (sccs (system-dependency-graph promesa-system))))
  )

(comment
  (def system2
    '{:a {:start 5}
      :b {:start (inc (high/ref :a))}})

  (run
    starting
    (map #(find system2 (first %))
         (sccs (system-dependency-graph system2)))))

(comment
  (cycles {:a #{:b}
           :b #{:a}
           :c #{:b :a}})
  (cycles (sccs {:a #{:b}
                 :b #{:a}
                 :c #{:b :a}}))

  (let [g {1 #{2}
           2 #{3}
           3 #{1}
           4 #{3 2 5}
           5 #{4 6}
           6 #{7 3}
           7 #{6}
           8 #{7 8}}]
    (cycles (sccs g) g)))
