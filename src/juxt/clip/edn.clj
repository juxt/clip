(ns juxt.clip.edn
  (:refer-clojure :exclude [load]))

(defmulti ^:no-doc load-key
  (fn [k _v] k))

(defmethod load-key :default
  [_k v]
  v)

(defmethod load-key :executor
  [_k executor]
  (if (fn? executor)
    executor
    (requiring-resolve executor)))

(defn load
  [system-config]
  (reduce-kv
    (fn [system-config k v]
      (assoc system-config k (load-key k v)))
    system-config
    system-config))
