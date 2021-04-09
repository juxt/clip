(ns juxt.clip.repl
  "REPL utilities for running a system during development."
  (:require
    [clojure.tools.namespace.repl :as tns.repl]
    [juxt.clip.core :as clip]
    [juxt.clip.impl.core :as impl]))

(tns.repl/disable-reload!)

(def ^:private default-initializer
  (fn [& args]
    (throw (Error. "No function to get system config found.  Did you forget to run `set-init!`?"))))

(def system nil)
(def ^:private initializer default-initializer)

(defn- stop-system
  [system]
  (when system
    (let [{::keys [deref] :as system-config} (::system-config (meta system))]
      (deref (clip/stop system-config system))))
  nil)

(defn- resolve-reloader
  [x]
  (if (symbol? x)
    (requiring-resolve x)
    x))

(defmacro ^:private get-macro
  "Like get, but not-found is only evaluated if k is not present in m"
  [map key not-found]
  `(let [m# ~map
         k# ~key]
     (if-let [[_# v#] (find m# k#)]
       v#
       ~not-found)))

(defn- reloading-f
  [reloads components]
  (fn [[k {:keys [start]}]]
    (fn [rf acc]
      (let [started (get acc k)
            make-v (fn
                     []
                     (try
                       (binding [impl/*running-system* acc
                                 impl/*components* components]
                         (impl/metacircular-evaluator
                           start
                           {'clip/ref (impl/clip-ref-fn components acc)}))
                       (catch Exception e
                         (binding [*out* *err*]
                           (println "Unable to reload" k "returning original value")
                           (clojure.stacktrace/print-throwable e))
                         started)))
            reloader
            (resolve-reloader
              (get-macro reloads
                         k
                         (reduce-kv
                           (fn [_ k reloader]
                             (when (and (class? k) (instance? k started))
                               (reduced reloader)))
                           nil
                           reloads)))]
        (if reloader
          (rf acc k (reloader make-v))
          acc)))))

(defn relodable-fn
  "ALPHA: A reload function suitable for use with clojure.lang.Fn in :reloads."
  [fn-fn]
  (fn reloadable-wrapper [& args]
    (apply (fn-fn) args)))

(defn- repl-start
  "Like clip/start, but has reload functionality built-in"
  [system-config]
  (let [{:keys [components executor reloads]
         :or {executor impl/exec-queue}} system-config
        executor (#'clip/->executor executor)
        [_ component-chain] (#'clip/safely-derive-parts components [])]
    (executor
      (for [component component-chain
            f [(impl/pre-starting-f components)
               (impl/starting-f components)
               (reloading-f (zipmap (map (fn [x]
                                           (if (symbol? x)
                                             (Class/forName (str x))
                                             x))
                                         (keys reloads))
                                    (vals reloads))
                            components)
               (impl/post-starting-f components)]]
        (f component)))))

(defn- start-system
  [system-config]
  (let [{::keys [deref]} system-config]
    (vary-meta
      (try
        (deref (repl-start system-config))
        (catch Throwable t
          (if-let [system (::clip/system (ex-data t))]
            ;; Partially started system found, we should call stop on it to clean
            ;; up.
            (do
              (try
                (clip/stop system-config system)
                (catch Throwable stop-t
                  (throw
                    (ex-info
                      "Exception thrown while starting system.  Exception also thrown while stopping system."
                      {:stop-exception stop-t}
                      t))))
              (throw (ex-cause t)))
            (throw t))))
      merge {::system-config system-config})))

(defn- force*
  "Like force, but for anything that can be deref'd"
  [ref]
  (if
    (or (instance? clojure.lang.IBlockingDeref ref)
        (instance? java.util.concurrent.Future ref))
    (deref ref)
    ref))

(defn- ->deref
  [deref]
  (if (fn? deref)
    deref
    (requiring-resolve deref)))

(defn set-init!
  "Set the initializer to init-fn.  Should be a function which takes no
  arguments and returns the system config to use for the REPL."
  [init-fn]
  (alter-var-root #'initializer (constantly #(update (merge {::deref force*} (init-fn)) ::deref ->deref))))

(defn start
  "Stops any existing systems and starts a new one calling the initializer set
  by set-init!"
  []
  (alter-var-root
    #'system
    (fn [system]
      (stop-system system)
      (start-system (initializer))))
  :started)

(defn ^:deprecated go
  "Calls `start`.  Exists for compatibility with other component libraries."
  []
  (start))

(defn stop
  "Stop the running system (if running)"
  []
  (alter-var-root #'system stop-system)
  :stopped)

(defn reset
  "Stop the running system, refresh using tools.namespace, and then start the
  new system."
  []
  (stop)
  (tns.repl/refresh :after `start))

(defn reset-all
  "Stop the running system, refresh-all using tools.namespace, and then start
  the new system."
  []
  (stop)
  (tns.repl/refresh-all :after `start))
