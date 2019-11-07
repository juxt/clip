(ns juxt.clip.repl
  "REPL utilities for running a system during development."
  (:require
    [clojure.tools.namespace.repl :as tns.repl]
    [juxt.clip.core :as clip]))

(tns.repl/disable-reload!)

(def ^:private default-initializer
  (fn [& args]
    (throw (Error. "No function to get system config found.  Did you forget to run `set-init!`?"))))

(def system nil)
(def ^:private initializer default-initializer)

(defn- stop-system
  [system]
  (when system
    (let [system-config (::system-config (meta system))]
      (clip/stop system-config system)
      system)))

(defn- start-system
  [system-config]
  (vary-meta
    (try
      (clip/start system-config)
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
    merge {::system-config system-config}))

(defn set-init!
  "Set the initializer to init-fn.  Should be a function which takes no
  arguments and returns the system config to use for the REPL."
  [init-fn]
  (alter-var-root #'initializer (constantly init-fn)))

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
