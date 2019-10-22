(ns io.dominic.high.impl.core
  (:require [clojure.set :as set]))

(defn sccs
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

(defn cycles
  [sccs g]
  (filter #(or (>= (count %) 2)
               (get-in g [(first %) (first %)]))
          sccs))

(defn system-dependency-graph
  [system]
  (let [ref? #(= 'high/ref (and (coll? %) (first %)))
        ref-to second]
    (into {}
          (map (fn [[k v]]
                 [k (set (map ref-to (filter ref? (tree-seq coll? seq (:start v)))))])
               system))))

(defn dependency-errors
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

(defn human-render-dependency-error
  [dependency-error]
  (case (:type dependency-error)
    :missing
    (str (:from dependency-error) " depends on " (:to dependency-error)
         ", but " (:to dependency-error) " doesn't exist.")

    :cycle
    (str "There's a circular dependency between "
         (apply str (interpose
                      " -> "
                      (concat
                        (reverse (:between dependency-error))
                        [(first (reverse (:between dependency-error)))]))))

    (pr-str dependency-error)))

(comment
  (let [bad-system '{:a {:start (high/ref :does-not-exist)}
                     :b {:start (high/ref :c)}
                     :c {:start (high/ref :b)}
                     
                     :d {:start (high/ref :e)}
                     :e {:start (high/ref :f)}
                     :f {:start (high/ref :d)}}]
    (map
      (or #_identity human-render-dependency-error)
      (dependency-errors
        (sccs (system-dependency-graph bad-system))
        (system-dependency-graph bad-system)))))

(defn namespace-symbol
  "Returns symbol unchanged if it has a namespace, or with clojure.core as it's
  namespace otherwise."
  [sym]
  (if (namespace sym)
    sym
    (symbol "clojure.core" (name sym))))

(defn evaluate-pseudo-clojure
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

(defn ref?
  [x]
  (= 'high/ref (and (coll? x) (first x))))

(def ^:private ref-to second)

(defn resolver
  [x p? lookup]
  (clojure.walk/postwalk
    (fn [x]
      (if (p? x) (lookup x) x))
    x))

(defn resolve-refs
  [x components running-system]
  (resolver x
            ref?
            (fn [ref-list]
              (let [to (ref-to ref-list)]
                (cond->> (get running-system to)

                  (get-in components [to :resolve])
                  (evaluate-pseudo-clojure
                    (get-in components [to :resolve])))))))

(defn pre-starting
  [rf]
  (fn
    ([running-system components id value]
     (let [component (get components id)]
       (when (contains? component :pre-start)
         (evaluate-pseudo-clojure
           (resolve-refs
             (get component :pre-start)
             components
             running-system))))
     (try
       (rf running-system components id value)
       (catch Throwable e
         (throw
           (ex-info
             "Error while creating system"
             {:partial-system running-system}
             e)))))))

(defn starting
  [rf]
  (fn
    ([running-system components id _]
     (let [component (get components id)
           resolved-start
           (resolve-refs (get component :start)
                         components
                         running-system)]
       (try
         (rf running-system
             components
             id
             (evaluate-pseudo-clojure resolved-start))
         (catch Throwable e
           (throw
             (ex-info
               "Error while creating system"
               {:partial-system running-system}
               e))))))))

(defn post-starting
  [rf]
  (fn
    ([running-system components id started]
     (try
       (let [component (get components id)]
         (when (contains? component :post-start)
           (evaluate-pseudo-clojure
             (-> (get component :post-start)
                 (resolve-refs components running-system)
                 (resolver #(= 'this %) (constantly started)))
             started))
         (rf running-system components id started))
       (catch Throwable e
         (throw
           (ex-info
             "Error while creating system"
             {:partial-system running-system}
             e)))))))

(defn stop!
  [inst stop-code]
  (cond
    stop-code
    (evaluate-pseudo-clojure
      (-> stop-code
          (resolver #(= 'this %) (constantly inst)))
      inst)
    (isa? (class inst) java.io.Closeable)
    (.close ^java.io.Closeable inst)))

(defn stopping
  [rf]
  (fn
    ([running-system components id v]
     (try
       (stop! (get running-system id)
              (get-in components [id :stop]))
       (rf running-system components id v)
       (catch Throwable e
         (throw
           (ex-info
             "Error while stopping system"
             {:partial-system running-system}
             e)))))))

(defn run
  ([xf components component-chain]
   (run xf {} components component-chain))
  ([xf init components component-chain]
   (let [f (xf (fn [acc _ id v] (conj acc [id v])))]
     (reduce (fn [init [id component]]
               (f init components id (get init id)))
             init
             component-chain))))

(defn promesa-run
  ([xfs components component-chain]
   (promesa-run xfs components ((requiring-resolve 'promesa.core/promise) {}) component-chain))
  ([xfs components init component-chain]
   (let [promise (requiring-resolve 'promesa.core/promise)
         then (requiring-resolve 'promesa.core/then)
         promesa-wait
         (fn [rf]
           (fn [acc components id value]
             (-> value (then #(rf acc components id %)))))
         xf (apply comp (interpose promesa-wait xfs))
         f (xf
             (fn [acc components id value]
               (-> value
                   (then (fn [value]
                           (assoc acc id value))))))]
     (reduce
       (fn [prom-chain [id component]]
         (-> prom-chain
             (then (fn [acc] (f acc components id (get acc id))))))
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
     promesa-system
     (promesa-run [pre-starting starting post-starting]
                  promesa-system
                  (component-chain promesa-system))
     (reverse-component-chain promesa-system))
  )

(defn component-chain
  [system]
  (map #(find system (first %))
       (sccs (system-dependency-graph system))))

(defn reverse-component-chain
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

(defn starting-f
  [components]
  (fn [[k {:keys [start]}]]
    (fn [rf acc]
      (rf acc
          k
          (evaluate-pseudo-clojure
            (resolve-refs start components acc))))))

(defn pre-starting-f
  [components]
  (fn [[k {:keys [pre-start]}]]
    (fn [rf acc]
      (when pre-start
        (evaluate-pseudo-clojure
          (resolve-refs pre-start components acc)))
      acc)))

(defn post-starting-f
  [components]
  (fn [[k {:keys [post-start]}]]
    (fn [rf acc]
      (when post-start
        (evaluate-pseudo-clojure
          (-> post-start
              (resolve-refs components acc)
              (resolver #(= 'this %) (constantly (get acc k))))
          (get acc k)))
      acc)))

(defn stopping-f
  [[k {:keys [stop]}]]
  (fn [rf acc]
    (stop! (get acc k) stop)
    (-> acc
        (rf (str k "-stop1") :value1)
        (rf (str k "-stop2") :value2))))

(defn exec-queue
  ([q] (exec-queue q {}))
  ([q init]
   (reduce (fn [acc f] (f assoc acc)) init q)))

(defn promesa-exec-queue
  ([q] (promesa-exec-queue
         q
         ((requiring-resolve 'promesa.core/resolved) {})))
  ([q init]
   (let [all (requiring-resolve 'promesa.core/all)
         then (requiring-resolve 'promesa.core/then)]
     (reduce
       (fn [*acc f]
         (-> *acc
             (then (fn [acc]
                     (f (fn [acc k v]
                          (-> (all [acc k v])
                              (then #(apply assoc %))))
                        acc)))))
       init
       q))))

(comment
  (exec-queue
    (concat (map (starting-f system2) (component-chain system2))
            (map stopping-f (reverse-component-chain system2))))

  @(promesa-exec-queue
     (concat
       (for [f [(pre-starting-f system2)
                (starting-f system2)]
             component (component-chain system2)]
         (f component))
       (map stopping-f (reverse-component-chain system2)))))
