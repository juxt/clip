(ns juxt.clip.repl.yada
  "Yada integration with Clip"
  (:require
    [yada.handler :as yada.handler]
    [yada.resource :as yada.resource]
    [yada.yada :as yada]))

(defrecord DynamicHandler [resource-fn]
  clojure.lang.IFn
  (invoke [this req]
    (let [handler (yada/handler (resource-fn))]
      (handler req)))
  yada.resource/ResourceCoercion
  (as-resource [this]
    (yada/as-resource (yada/handler (resource-fn))))
  ;; satisfies all of the same protocols as a handler
  yada.handler/HandlerCoercion
  (as-handler [this] this))

(defn reloadable-resource
  "A reload for clip which will convert a yada resource into a handler
  which will re-generate the resource on each request."
  [resource-fn]
  ;; Convert resources straight to a handler, as there's no way to make
  ;; a dynamic resource
  (->DynamicHandler resource-fn))
