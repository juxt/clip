(ns juxt.clip.promesa
  (:require
    [promesa.core :as p]))

(defn exec
  ([q] (exec q (p/resolved {})))
  ([q init]
   (reduce
     (fn [*acc f]
       (-> *acc
           (p/then
             (fn [acc]
               (try
                 (f (fn [acc k v]
                      (-> (p/all [acc k v])
                          (p/then #(apply assoc %))
                          (p/catch
                            (fn [e]
                              (throw (ex-info "Failure while executing on system" 
                                              {:juxt.clip.core/type :thrown
                                               :juxt.clip.core/system acc
                                               :juxt.clip.core/unapplied-v v}
                                              e))))))
                    acc)
                 (catch #?(:clj Throwable :cljs js/Error) t
                   (throw (ex-info "Failure while executing on system" 
                                   {:juxt.clip.core/type :thrown
                                    :juxt.clip.core/system acc}
                                   t))))))))
     init
     q)))

(comment
  (def system2
    {:foo {:start `(p/resolved 10)}
     :bar {:start '(inc (clip/ref :foo))}})
  (require '[juxt.clip.impl.core :as impl])
  @(exec
     (concat
       (for [f [(impl/pre-starting-f system2)
                (impl/starting-f system2)]
             component (impl/component-chain system2)]
         (f component))
       (map impl/stopping-f (impl/reverse-component-chain system2))))
  )
