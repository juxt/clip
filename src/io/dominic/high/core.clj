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
  [x ref-lookup]
  (resolver x ref? #(get ref-lookup (ref-to %))))

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
     (list? x)
     (apply (requiring-resolve (namespace-symbol (first x)))
            (map evaluate-pseudo-clojure (rest x)))
     ;; simple values are useful for testing, not so useful in real systems.
     (or (number? x) (string? x) (keyword? x))
     x
     :else (throw (ex-info (str "`" (pr-str x) "` is not a valid high code form.")
                           {:x x}))))

  ([x implicit-target]
   (cond
     (symbol? x)
     ((requiring-resolve (namespace-symbol x)) implicit-target)
     (list? x)
     (apply (requiring-resolve (namespace-symbol (first x)))
            (map evaluate-pseudo-clojure (rest x)))
     ;; simple values are useful for testing, not so useful in real systems.
     (or (number? x) (string? x) (keyword? x))
     x
     :else (throw (ex-info (str "`" (pr-str x) "` is not a valid high code form.")
                           {:x x})))))

(defn- pre-starting
  [rf]
  (fn
    ([running-system [_ component :as entry]]
     (when (contains? component :pre-start)
       (evaluate-pseudo-clojure
         (resolve-refs
           (get component :pre-start)
           running-system)))
     (try
       (rf running-system entry)
       (catch Throwable e
         (throw
           (ex-info
             "Error while creating system"
             {:partial-system running-system}
             e)))))))

(defn- starting
  [rf]
  (fn
    ([running-system [id component]]
     (let [resolved-start (resolve-refs (get component :start) running-system)]
       (try
         (rf running-system [id (evaluate-pseudo-clojure resolved-start)])
         (catch Throwable e
           (throw
             (ex-info
               "Error while creating system"
               {:partial-system running-system}
               e))))))))

(defn- post-starting
  [rf]
  (fn
    ([running-system entry]
     (try
       (rf running-system entry)
       (catch Throwable e
         (throw
           (ex-info
             "Error while creating system"
             {:partial-system running-system}
             e)))))))

(defn- stopping
  [rf]
  (fn
    ([running-system [id component :as entry]]
     (let [inst (get running-system id)]
       (cond
         (get component :stop)
         (evaluate-pseudo-clojure
           (-> (get component :stop)
               (resolver #(= 'this %) (constantly inst)))
           inst)

         (some-> (get running-system id)
                 class
                 (isa? java.io.Closeable))
         (.close ^java.io.Closeable inst)))
     (try
       (rf running-system entry)
       (catch Throwable e
         (throw
           (ex-info
             "Error while stopping system"
             {:partial-system running-system}
             e)))))))

(defn- run
  ([xf component-chain]
   (run xf {} component-chain))
  ([xf init component-chain]
   (let [f (xf conj)]
     (reduce f init component-chain))))

(defn- promesa-run
  ([xfs component-chain]
   (promesa-run xfs ((requiring-resolve 'promesa.core/promise) {}) component-chain))
  ([xfs init component-chain]
   (let [promise (requiring-resolve 'promesa.core/promise)
         then (requiring-resolve 'promesa.core/then)
         promesa-wait
         (fn [rf]
           (let [then (requiring-resolve 'promesa.core/then)]
             (fn [acc [id value]]
               (-> value (then #(rf acc [id %]))))))
         xf (apply comp (interpose promesa-wait xfs))
         f (xf
             (fn [acc [id value :as entry]]
               (-> value
                   (then (fn [value]
                           (assoc acc id value))))))]
     (reduce
       (fn [prom-chain component]
         (-> prom-chain
             (then (fn [acc] (f acc component)))))
       init
       component-chain))))

(comment
  (def promesa-system
    '{foo {:start (io.dominic.high.core/StatefulThing)
           :pre-start (println "aaaa")}
      :a {:pre-start (println 10)
          :start (promesa.core/resolved 5)}
      :num {:start 10
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
          :pre-start (println "bbbb")}})

  (run
    stopping
    (run (comp pre-starting starting post-starting)
         (component-chain system2))
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
