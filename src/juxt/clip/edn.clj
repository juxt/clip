(ns juxt.clip.edn)

;; TODO: Make public so extensions can decide how to load their keys.
(defmulti ^:private edn->clj
  (fn [k _v] k))

(defmethod edn->clj :default
  [_k v]
  v)

(defmethod edn->clj :executor
  [_k executor]
  (if (fn? executor)
    executor
    (requiring-resolve executor)))

(defn analyze
  [system-config]
  (reduce-kv
    (fn [system-config k v]
      (assoc system-config k (edn->clj k v)))
    system-config
    system-config))
