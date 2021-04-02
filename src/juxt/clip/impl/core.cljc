(ns juxt.clip.impl.core
  (:require [clojure.walk :as walk]))

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

(defn ref?
  [x]
  (or (= 'clip/ref (and (coll? x) (first x)))
      (= 'juxt.clip.core/ref (and (coll? x) (first x)))))

(def ^:private ref-to second)

(defn system-dependency-graph
  [system]
  (into {}
        (map (fn [[k v]]
               [k (set
                    (concat
                      (->> (:start v)
                           (tree-seq coll? seq)
                           (filter ref?)
                           (map ref-to))
                      (:juxt.clip.core/deps
                        (meta (:start v)))))])
             system)))

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
  (let [bad-system '{:a {:start (clip/ref :does-not-exist)}
                     :b {:start (clip/ref :c)}
                     :c {:start (clip/ref :b)}
                     
                     :d {:start (clip/ref :e)}
                     :e {:start (clip/ref :f)}
                     :f {:start (clip/ref :d)}}]
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

#?(:clj
   (defn symbol->f
     [sym]
     (if (= \. (first (str sym)))
       #(clojure.lang.Reflector/invokeInstanceMethod
          %1
          (subs (str sym) 1)
          (into-array Object %&))
       (requiring-resolve (namespace-symbol sym)))))

(defn metacircular-evaluator
  [form env]
  (let [form (if
               (or (keyword? form) (symbol? form) (fn? form))
               (if (contains? env 'this)
                 (list form 'this)
                 (list form))
               form)]
    (walk/postwalk
      (fn [x]
        (try
          (cond
            (and (symbol? x) (contains? env x))
            (get env x)


            #?@(:cljs []
                :default [(and (symbol? x)
                               (requiring-resolve (namespace-symbol x)))
                          (requiring-resolve (namespace-symbol x))])

            #?@(:clj [(and (symbol? x) (= \. (first (str x))))
                      #(clojure.lang.Reflector/invokeInstanceMethod
                                     %1
                                     (subs (str x) 1)
                                     (into-array Object %&))]
                :default [])

            (symbol? x)
            (throw (ex-info (str "Unable to resolve symbol " x)
                            {:form form
                             :expr x}))

            (seq? x)
            (apply (first x) (rest x))

            :else x)
          (catch Exception e
            (throw (ex-info (str "Unable to evaluate form " (pr-str x)) {:form x} e)))))
      form)))

(def ^:dynamic *running-system*)
(def ^:dynamic *components*)

(defn stop!
  [inst stop-code]
  (cond
    stop-code
    (metacircular-evaluator stop-code {'this inst})
    #?@(:clj
         [(isa? (class inst) java.io.Closeable)
          (.close ^java.io.Closeable inst)])))

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
    '{:a {:start (juxt.clip.core/StatefulThing)
          :pre-start (println "aaaa")}
      :num {:start 10
            :stop (println "num-->" this)}
      :b {:start (inc (clip/ref :num))
          :pre-start (println "bbbb")
          :post-start  (println "postpostpost" this)}}))

(comment
  (let [g {1 #{2}
           2 #{3}
           3 #{1}
           4 #{3 2 5}
           5 #{4 6}
           6 #{7 3}
           7 #{6}
           8 #{7 8}}]
    (cycles (sccs g) g)))

(defn clip-ref-fn
  [components running-system]
  (let [resolve-ref
        ;; TODO: there's some trick to having a self-referential memoized fn

        ;; TODO: memoize so we only run resolve once for each component, per xf
        (fn resolve-ref [to]
          (if-let [resolve-code (get-in components [to :resolve])]
            (metacircular-evaluator resolve-code {'this (get running-system to)
                                                  'clip/ref resolve-ref})
            (get running-system to)))]
    (fn [to]
      (resolve-ref to))))

(defn starting-f
  [components]
  (fn [[k {:keys [start]}]]
    (fn [rf acc]
      (rf acc
          k
          (binding [*running-system* acc
                    *components* components]
            (metacircular-evaluator start {'clip/ref (clip-ref-fn components acc)}))))))

(defn pre-starting-f
  [components]
  (fn [[_ {:keys [pre-start]}]]
    (fn [_ acc]
      (when pre-start
        (binding [*running-system* acc
                  *components* components]
          (metacircular-evaluator
            pre-start
            {'clip/ref (clip-ref-fn components acc)})))
      acc)))

(defn post-starting-f
  [components]
  (fn [[k {:keys [post-start]}]]
    (fn [_ acc]
      (when post-start
        (binding [*running-system* acc
                  *components* components]
          (metacircular-evaluator
            post-start
            {'clip/ref (clip-ref-fn components acc)
             'this (get acc k)})))
      acc)))

(defn stopping-f
  [[k {:keys [stop]}]]
  (fn [rf acc]
    (rf acc k (stop! (get acc k) stop))))

(defn exec-queue
  ([q] (exec-queue q {}))
  ([q init]
   (reduce
     (fn [acc f]
       (try
         (f assoc acc)
         (catch #?(:clj Throwable :cljs js/Error) t
           (throw (ex-info "Failure while executing on system"
                           {:juxt.clip.core/type :thrown
                            :juxt.clip.core/system acc}
                           t)))))
     init q)))

(comment
  (exec-queue
    (concat (map (starting-f system2) (component-chain system2))
            (map stopping-f (reverse-component-chain system2)))))
