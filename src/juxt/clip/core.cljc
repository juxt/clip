(ns juxt.clip.core
  (:refer-clojure :exclude [ref])
  (:require [juxt.clip.impl.core :as impl]
            [clojure.walk :as walk]))

(defn- safely-derive-parts
  [components init]
  (let [g (impl/system-dependency-graph components)
        sccs (impl/sccs g init)]
    (if-let [errors (seq (impl/dependency-errors sccs g))]
      (throw
        (ex-info
          (apply
            str
            "Could not construct execution order because:\n  "
            (interpose "\n  " (map impl/human-render-dependency-error errors)))
          {:components components
           :errors errors
           :g g
           :sccs sccs}))

      [g (map #(find components (first %)) sccs)])))

(defn- ->executor
  [executor]
  #?(:cljs executor
     :default
     (if (fn? executor)
       executor
       (requiring-resolve executor))))

(defn start
  "Takes a system config to start.  Returns a running system where the keys map
  to the started component.  Runs the :pre-start, :start and :post-start keys
  in that order.
  
  :pre-start and :start may contains references, and will be executed without
  an implicit target.
  :post-start may also contain references, but will have the started component
  as an implict target.  This means that a symbol on it's own will work instead
  of requiring a code form, in addition the anaphoric variable `this` is
  available to refer to the started component."
  [system-config]
  (let [{:keys [components executor]
         :or {executor impl/exec-queue}} system-config
        executor (->executor executor)
        [_ component-chain] (safely-derive-parts components [])]
    (executor
      (for [component component-chain
            f [(impl/pre-starting-f components)
               (impl/starting-f components)
               (impl/post-starting-f components)]]
        (f component)))))

(defn stop
  "Takes a system config to stop.

  Runs the :stop key, which may not contain references, and will be executed
  with the started component as an implict target.  If no :stop is provided and
  the target is AutoClosable then .close will be called on it, otherwise
  nothing."
  [system-config running-system]
  (let [{:keys [components executor]
         :or {executor impl/exec-queue}} system-config
        executor (->executor executor)
        [_ component-chain] (safely-derive-parts
                              (select-keys components (keys running-system))
                              ())]
    (executor
      (map impl/stopping-f component-chain)
      running-system)))

(comment
  (start
    {:components '{:foo {:start (clip/ref :bar)}
                   :bar {:start (clip/ref :foo)}
                   :baz {:start (clip/ref :baz)}
                   :foob {:start (clip/ref :nowhere)}}
     :executor impl/exec-queue})

  (let [system-config {:components '{:foo {:pre-start (prn "init:foo")
                                           :start 1
                                           :post-start prn
                                           :stop prn}
                                     :bar {:start (inc (clip/ref :foo))
                                           :stop prn}}
                       :executor impl/promesa-exec-queue}]
    (stop system-config (start system-config))))

(def exec-sync
  "Executor which runs sync between calls."
  impl/exec-queue)

(defn ref
  [ref-to]
  (list 'clip/ref ref-to))

(defn- deval-body
  "Takes a body of code and defers evaluation of lists."
  [body]
  (walk/postwalk
    (fn [x]
      (if (and (list? x)
               (not= (first x) 'clip/ref))
        (cons `list x)
        x))
    body))

(defmacro deval
  "EXPERIMENTAL.  Defers evaluation of lists.  Useful for defining component
  start/stop/etc. functions."
  [body]
  (deval-body body))

#?(:clj
   (defmacro with-deps
     "Takes bindings and a body, like `fn`. The first binding must be to deps, and
     be a destructuring of the deps you want.

     ```
     (with-deps [{:keys [foo bar]}]
     (+ foo bar))
     ```

     ```
     (with-deps [{foo :foo}]
     (str foo))
     ```

     Remaining bindings will be arguments to the generated function.  For example,
     :stop will provide you with the running instance to stop.

     ```
     (with-deps [{:keys [foo]} this]
     (.close this foo))
     ```"
     [[dep-binding & args] & body]
     (assert
       (map? dep-binding)
       "Please use a map to destructure the deps (known as associative destructuring)")
     (let [deps (mapv last
                      (filter
                        (fn [x]
                          (and (seq? x)
                               (= 'clojure.core/get (first x))))
                        (map second (partition 2 (destructure [dep-binding nil])))))]
       `(vary-meta
          ;; https://clojure.atlassian.net/browse/CLJ-2539
          ^::fix (fn self# ~(or (vec args) [])
                   (let [~dep-binding
                         (zipmap ~deps
                                 (map
                                   (fn [k#]
                                     (if-let [resolve-code# (get-in impl/*components* [k# :resolve])]
                                       (impl/metacircular-evaluator
                                         resolve-code#
                                         {~''this (get impl/*running-system* k#)})
                                       (get impl/*running-system* k#)))
                                   ~deps))]
                     ~@body))
          assoc
          ::deps
          ~deps))))
