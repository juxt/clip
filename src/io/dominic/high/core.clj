(ns io.dominic.high.core
  (:require
    [clojure.set :as set]))

(defn- sccs
  "Returns a topologically sorted list of strongly connected components.
  Tarjan's algorithm."
  ([g] (sccs g []))
  ([g sccs-init]
   (let [strong-connect
           (fn strong-connect [acc v]
             (let [acc (-> acc
                           (assoc-in [:idxs v] (:idx acc))
                           (assoc-in [:low-links v] (:idx acc))
                           (update :idx inc)
                           (update :S conj v)
                           (assoc-in [:on-stack v] true))
                   acc (reduce
                         (fn [acc w]
                           (cond
                             (not (get-in acc [:idxs w]))
                               (let [acc (strong-connect acc w)]
                                 (update-in acc
                                            [:low-links v]
                                            min
                                            (get-in acc [:low-links w])))
                             (get-in acc [:on-stack w])
                               (update-in acc
                                          [:low-links v]
                                          min
                                          (get-in acc [:idxs w]))
                             :else acc))
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
                       (assoc :S S
                              :on-stack on-stack)
                       (update :sccs conj scc)))
                 acc)))]
     (:sccs
       (reduce (fn [acc v]
                 (if-not (contains? (:idxs acc) v) (strong-connect acc v) acc))
         {:S ()
          :idx 0
          :sccs sccs-init}
         (keys g))))))

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

(defn- resolver
  [x p? lookup]
  (clojure.walk/postwalk
    (fn [x]
      (if (p? x) (lookup x) x))
    x))

(defn- resolve-refs
  ([x ref-lookup]
   (resolve-refs x nil ref-lookup))
  ([x resolve-ref ref-lookup]
   (let [resolve-ref (or resolve-ref identity)]
     (resolver x ref? #(resolve-ref (get ref-lookup (ref-to %)))))))

(defn- namespace-symbol
  "Returns symbol unchanged if it has a namespace, or with clojure.core as it's
  namespace otherwise."
  [sym]
  (if (namespace sym)
    sym
    (symbol "clojure.core" (name sym))))

(defn- evaluate-pseudo-clojure
  ([x]
   (cond
     (symbol? x)
     ((requiring-resolve (namespace-symbol x)))
     (sequential? x)
     (apply (if (symbol? (first x))
              (requiring-resolve (namespace-symbol (first x)))
              (evaluate-pseudo-clojure (first x)))
            (map evaluate-pseudo-clojure (rest x)))
     :else x))

  ([x implicit-target]
   (cond
     (symbol? x)
     ((requiring-resolve (namespace-symbol x)) implicit-target)

     (keyword? x)
     (get implicit-target x)

     :else (evaluate-pseudo-clojure x))))

(defn- pre-starting
  [rf]
  (fn
    ([running-system system-config id value]
     (let [component (get system-config id)]
       (when (contains? component :pre-start)
         (evaluate-pseudo-clojure
           (resolve-refs
             (get component :pre-start)
             (some->> (get component :resolve)
                      (partial evaluate-pseudo-clojure))
             running-system))))
     (try
       (rf running-system system-config id value)
       (catch Throwable e
         (throw
           (ex-info
             "Error while creating system"
             {:partial-system running-system}
             e)))))))

(defn- starting
  [rf]
  (fn
    ([running-system system-config id _]
     (let [component (get system-config id)
           resolved-start
           (resolve-refs (get component :start)
                         (some->> (get component :resolve)
                                  (partial evaluate-pseudo-clojure))
                         running-system)]
       (try
         (rf running-system
             system-config
             id
             (evaluate-pseudo-clojure resolved-start))
         (catch Throwable e
           (throw
             (ex-info
               "Error while creating system"
               {:partial-system running-system}
               e))))))))

(defn- post-starting
  [rf]
  (fn
    ([running-system system-config id started]
     (try
       (let [component (get system-config id)]
         (when (contains? component :post-start)
           (evaluate-pseudo-clojure
             (-> (get component :post-start)
                 (resolve-refs
                   (some->> (get component :resolve)
                            (partial evaluate-pseudo-clojure))
                   running-system)
                 (resolver #(= 'this %) (constantly started)))
             started))
         (rf running-system system-config id started))
       (catch Throwable e
         (throw
           (ex-info
             "Error while creating system"
             {:partial-system running-system}
             e)))))))

(defn- stopping
  [rf]
  (fn
    ([running-system system-config id v]
     (let [inst (get running-system id)
           component (get system-config id)]
       (cond
         (get component :stop)
         (evaluate-pseudo-clojure
           (-> (get component :stop)
               (resolver #(= 'this %) (constantly inst)))
           inst)

         (some-> (get running-system id)
                 class
                 (isa? java.io.Closeable))
         (.close ^java.io.Closeable inst))
       (try
         (rf running-system system-config id v)
         (catch Throwable e
           (throw
             (ex-info
               "Error while stopping system"
               {:partial-system running-system}
               e))))))))

(defn- run
  ([xf system-config component-chain]
   (run xf {} system-config component-chain))
  ([xf init system-config component-chain]
   (let [f (xf (fn [acc _ id v] (conj acc [id v])))]
     (reduce (fn [init [id component]]
               (f init system-config id (get init id)))
             init
             component-chain))))

(defn- promesa-run
  ([xfs component-chain]
   (promesa-run xfs ((requiring-resolve 'promesa.core/promise) {}) component-chain))
  ([xfs init component-chain]
   (let [promise (requiring-resolve 'promesa.core/promise)
         then (requiring-resolve 'promesa.core/then)
         promesa-wait
         (fn [rf]
           (let [then (requiring-resolve 'promesa.core/then)]
             (fn [acc id component value]
               (-> value (then #(rf acc id component %))))))
         xf (apply comp (interpose promesa-wait xfs))
         f (xf
             (fn [acc id component value]
               (-> value
                   (then (fn [value]
                           (assoc acc id value))))))]
     (reduce
       (fn [prom-chain [id component]]
         (-> prom-chain
             (then (fn [acc] (f acc id component nil)))))
       init
       component-chain))))

(comment
  (def promesa-system
    '{foo {:start (io.dominic.high.core/StatefulThing)
           :pre-start (println "aaaa")}
      :a {:pre-start (println 10)
          :start (promesa.core/resolved 5)}
      :num {:start 10
            :post-start (println "init-:num" (inc this))
            :stop (println "num-->" this)}
      :b {:start (promesa.core/resolved (inc (high/ref :a)))}
      :c {:start (+ (high/ref :a) (high/ref :b))}})

  @(promesa-run
     [stopping]
     @(promesa-run [pre-starting starting post-starting]
                   (component-chain promesa-system))
     (reverse-component-chain promesa-system))
  )

(defn- component-chain
  [system]
  (map #(find system (first %))
       (sccs (system-dependency-graph system))))

(defn- reverse-component-chain
  [system]
  (map #(find system (first %))
       (sccs (system-dependency-graph system) ())))

(comment
  (defn StatefulThing
    []
    (reify
      java.io.Closeable
      (close [this]
        (println  "Closing a stateful thing"))))
  (def system2
    '{:a {:start (io.dominic.high.core/StatefulThing)
          :pre-start (println "aaaa")}
      :num {:start 10
            :stop (println "num-->" this)}
      :b {:start (inc (high/ref :num))
          :pre-start (println "bbbb")
          :post-start  (println "postpostpost" this)}})

  (run starting
       system2
       (component-chain system2))

  (run
    stopping
    (run (comp pre-starting starting post-starting)
         system2
         (component-chain system2))
    system2
    (reverse-component-chain system2)))

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
