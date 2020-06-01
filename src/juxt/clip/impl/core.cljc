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

(defprotocol IPreventEval
  (-get-value [this] "Return value that has been blocked from evaluation"))

(defn- prevent-eval
  [x]
  (reify
    IPreventEval
    (-get-value [_] x)
    Object
    (toString [_]
      (str "#blocked-eval [" (.toString x) "]"))))

(defn- blocked-eval?
  [x]
  (satisfies? IPreventEval x))

(defn- get-value
  [x]
  (loop [x x]
    (if (blocked-eval? x)
      (recur (-get-value x))
      x)))

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

(defn evaluate-pseudo-clojure
  ([form]
   (let [form (if (or (keyword? form) (symbol? form) (fn? form))
                (list form)
                form)]
     (walk/postwalk
       get-value
       (walk/postwalk
         (fn [x]
           (if (seq? x)
             (prevent-eval
               (apply #?(:cljs (first x)
                         :default (cond
                                    (symbol? (first x))
                                    (if-let [f (symbol->f (first x))]
                                      f
                                      (throw
                                        (ex-info
                                          (str "Got null for function looking up symbol: "
                                               (first x))
                                          {})))

                                    (ifn? (first x))
                                    (first x)

                                    :else
                                    (throw (ex-info (str "Unsupported callable: " (pr-str (first x)))
                                                    {::callable (first x)
                                                     ::form x}))))
                      #?(:cljs (rest x)
                         :default (map (fn [x]
                                         (if (symbol? x)
                                           (requiring-resolve (namespace-symbol x))
                                           (get-value x)))
                                       (rest x)))))

             (get-value x)))
         form))))
  ([form implicit-target]
   (evaluate-pseudo-clojure
     (if (or (keyword? form) (symbol? form) (fn? form))
       (list form (prevent-eval implicit-target))
       form))))

(defn resolver
  [x p? lookup]
  (walk/postwalk
    (fn [x]
      (if (p? x) (prevent-eval (lookup x)) x))
    x))

(def ^:dynamic *running-system*)

(defn resolve-refs
  [x components running-system]
  (walk/postwalk
    (fn [x]
      (cond
        (ref? x)
        (let [to (ref-to x)]
          (cond->> (prevent-eval (get running-system to))

            (get-in components [to :resolve])
            (evaluate-pseudo-clojure
              (get-in components [to :resolve]))

            (get-in components [to :resolve])
            prevent-eval))

        #_#_(:juxt.clip.core/deps (meta x))
        (vary-meta x
                   assoc
                   :juxt.clip.core/resolved-deps
                   (zipmap (:juxt.clip.core/deps (meta x))
                           ;; TODO: :resolve
                           (map
                             #(get running-system %)
                             (:juxt.clip.core/deps (meta x)))))

        :else
        x))
    x))

(defn stop!
  [inst stop-code]
  (cond
    stop-code
    (evaluate-pseudo-clojure
      (-> stop-code
          (resolver #(= 'this %) (constantly inst)))
      inst)
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

(defn starting-f
  [components]
  (fn [[k {:keys [start]}]]
    (fn [rf acc]
      (rf acc
          k
          (binding [*running-system* acc]
            (evaluate-pseudo-clojure
              (resolve-refs start components acc)))))))

(defn pre-starting-f
  [components]
  (fn [[_ {:keys [pre-start]}]]
    (fn [_ acc]
      (when pre-start
        (binding [*running-system* acc]
          (evaluate-pseudo-clojure
            (resolve-refs pre-start components acc))))
      acc)))

(defn post-starting-f
  [components]
  (fn [[k {:keys [post-start]}]]
    (fn [_ acc]
      (when post-start
        (binding [*running-system* acc]
          (evaluate-pseudo-clojure
            (-> post-start
                (resolve-refs components acc)
                (resolver #(= 'this %) (constantly (get acc k))))
            (get acc k))))
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
