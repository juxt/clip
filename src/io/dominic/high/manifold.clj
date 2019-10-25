(ns io.dominic.high.manifold
  (:require
    [manifold.deferred :as d]))

(defn exec
  ([q] (exec q {}))
  ([q init]
   (reduce
     (fn [*acc f]
       (-> *acc
           (d/chain
             (fn [acc]
               (try
                 (f (fn [acc k v]
                      (d/catch
                        (d/chain (d/zip acc k v)
                                 #(apply assoc %))
                        (fn [e]
                          (throw (ex-info "Failure while executing on system" 
                                          {:io.dominic.high.core/type :thrown
                                           :io.dominic.high.core/system acc
                                           :io.dominic.high.core/unapplied-v v}
                                          e)))))
                    acc)
                 (catch Throwable t
                   (throw (ex-info "Failure while executing on system" 
                                   {:io.dominic.high.core/type :thrown
                                    :io.dominic.high.core/system acc}
                                   t))))))))
     init
     q)))

(comment
  (def system2
    {:foo {:start `(d/chain 10)}
     :bar {:start '(inc (high/ref :foo))}})
  (require '[io.dominic.high.impl.core :as impl])
  @(exec
     (concat
       (for [f [(impl/pre-starting-f system2)
                (impl/starting-f system2)]
             component (impl/component-chain system2)]
         (f component))
       (map impl/stopping-f (impl/reverse-component-chain system2)))
     {:x 10}))
