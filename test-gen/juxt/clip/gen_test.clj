(ns juxt.clip.gen-test
  (:require
    [clojure.edn :as edn]
    [clojure.test.check :as check]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [juxt.clip.core :as clip]))

(def ^:private evaluation-gen
  (gen/recursive-gen
    (fn [inner-gen]
      (gen/one-of
        [(gen/vector inner-gen)
         (gen/set inner-gen)
         (gen/fmap list* (gen/tuple (gen/return identity) inner-gen))
         (gen/map (gen/one-of [inner-gen gen/keyword gen/int])
                  inner-gen)]))
    (gen/fmap list*
              (gen/tuple (gen/one-of [(gen/return `inc) (gen/return inc) (gen/return 'inc)])
                         (gen/return 1)))))

(def evaluation-never-prevent
  (prop/for-all [y evaluation-gen]
    (let [started (::foo (clip/start {:components {::foo {:start y}}}))]
      (not (some @#'juxt.clip.impl.core/blocked-eval? (tree-seq seqable? seq started))))))

(defn -main
  [& args]
  (let [[num-tests & args] (map edn/read-string args)
        res (apply check/quick-check num-tests evaluation-never-prevent args)]
    (prn res)
    (System/exit (if (:pass? res) 0 1))))
