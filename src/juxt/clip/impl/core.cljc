(ns juxt.clip.impl.core
  (:require [clojure.walk :as walk]))

(defn sccs
  "Returns a topologically sorted list of strongly connected components.
  Tarjan's algorithm."
  ([g] (sccs g []))
  ([g sccs-init] (sccs g sccs-init (keys g)))
  ([g sccs-init ks]
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
         ks)))))

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

(defprotocol Evaluate
  (evaluate [x env]))

(extend-protocol Evaluate
  Object
  (evaluate [x _env] x)

  nil
  (evaluate [x _env] x))

(defrecord Analyzed [f]
  Evaluate
  (evaluate [_this env]
    (f env)))

(defn metacircular-analyzer
  [form locals]
  (let [form (if
               (or (keyword? form) (symbol? form) (fn? form))
               (if (contains? locals 'this)
                 (list form 'this)
                 (list form))
               form)]
    (walk/postwalk
      (fn [x]
        (cond
          (and (symbol? x) (contains? locals x))
          (->Analyzed (fn local-sym [env] (get env x)))

          #?@(:cljs []
              :default [(and (symbol? x)
                             (requiring-resolve (namespace-symbol x)))
                        (let [var (requiring-resolve (namespace-symbol x))]
                          (->Analyzed (fn resolved-var [_env] var)))])

          #?@(:clj [(and (symbol? x) (= \. (first (str x))))
                    (let [method (subs (str x) 1)]
                      #(clojure.lang.Reflector/invokeInstanceMethod
                         %1
                         method
                         (into-array Object %&)))]
              :default [])

          (symbol? x)
          (throw (ex-info (str "Unable to resolve symbol " x)
                          {:form form
                           :expr x
                           :locals locals}))

          :else x))
      form)))

(defn evaluator
  ([analysis env]
   (walk/postwalk
     (fn  [x]
       (if (seq? x)
         (apply (first x) (rest x))
         (evaluate x env)))
     analysis))
  ([analysis]
   (evaluator analysis nil)))

(def ^:dynamic *running-system*)

(defn stop!
  [inst stop-code]
  (cond
    stop-code
    (evaluator stop-code {'this inst})
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

(defn get-ref
  [running-system to]
  (if-let [r (get-in (meta running-system) [::resolve to])]
    @r
    (get running-system to)))

(defn clip-ref-fn
  [running-system]
  (fn [to]
    (get-ref running-system to)))

(defn starting-f
  [[k {:keys [start resolve]}]]
  (let [start-ana (metacircular-analyzer start '#{clip/ref juxt.clip.core/ref})
        resolve-ana (metacircular-analyzer resolve '#{this})]
    (fn [rf acc]
      (let [ref-fn (clip-ref-fn acc)
            v (binding [*running-system* acc]
                (evaluator start-ana
                           {'clip/ref ref-fn
                            'juxt.clip.core/ref ref-fn}))]
        (cond-> acc
          resolve-ana
          (vary-meta assoc-in [::resolve k]
                     (delay (evaluator resolve-ana
                                       {'clip/ref ref-fn
                                        'juxt.clip.core/ref ref-fn
                                        'this v})))
          true
          (rf k v))))))

(defn pre-starting-f
  [[_ {:keys [pre-start]}]]
  (if pre-start
    (let [pre-start-ana (metacircular-analyzer pre-start '#{clip/ref juxt.clip.core/ref})]
      (fn [_ acc]
        (binding [*running-system* acc]
          (evaluator
            pre-start-ana
            (let [ref-fn (clip-ref-fn acc)]
              {'clip/ref ref-fn
               'juxt.clip.core/ref ref-fn})))))
    (fn [_ acc] acc)))

(defn post-starting-f
  [[k {:keys [post-start]}]]
  (if post-start
    (let [post-start-ana (metacircular-analyzer
                           post-start
                           '#{clip/ref juxt.clip.core/ref this})]
      (fn [_ acc]
        (binding [*running-system* acc]
          (evaluator
            post-start-ana
            (let [ref-fn (clip-ref-fn acc)]
              {'clip/ref ref-fn
               'juxt.clip.core/ref ref-fn
               'this (get acc k)})))
        acc))
    (fn [_ acc]
      acc)))

(defn stopping-f
  [[k {:keys [stop]}]]
  (let [stop-ana (some-> stop (metacircular-analyzer #{'this}))]
    (fn [rf acc]
      (rf acc k (stop! (get acc k) stop-ana)))))

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
