(ns juxt.clip.core-test
  (:require
    [clojure.test :refer [deftest is are]]
    [juxt.clip.core :as clip]
    [clojure.edn :as edn]))

(deftest start
  (is (= {:foo 1 :bar 2}
         (clip/start
           {:components
            {:foo {:start 1}
             :bar {:start '(inc (clip/ref :foo))}}})))

  (is (= {nil 1 :bar 2}
         (clip/start
           {:components
            {nil {:start 1}
             :bar {:start '(inc (clip/ref nil))}}})))

  (is (= {:foo 1 :bar 3}
         (clip/start
           {:components
            {:foo {:start 1
                   :resolve inc}
             :bar {:start '(inc (clip/ref :foo))}}})))
  
  (is (= {:foo 1 :bar 3 :baz 4}
         (clip/start
           {:components
            {:foo {:start 1
                   :resolve inc}
             :bar {:start '(inc (clip/ref :foo))}
             :baz {:start (clip/with-deps
                            [{:keys [foo bar]}]
                            (+ foo bar))}}}))))

(deftest start-graph-ex
  (is (thrown? Throwable
               (clip/start
                 {:components
                  {:a {:start (clip/ref :b)}
                   :b {:start (clip/ref :a)}}})))
  (is (thrown? Throwable
               (clip/start
                 {:components
                  {:a {:start (clip/ref :b)}
                   :b {:start (clip/ref :c)}
                   :c {:start (clip/ref :a)}}})))
  (is (thrown? Throwable
               (clip/start
                 {:components
                  {:a {:start (clip/ref :b)}
                   :b {:start (clip/ref :fourohfour)}}}))))

(defn- reverse-contains?
  "contains? but with arguments switched for better `is` compatibility."
  [key coll]
  (contains? coll key))

(deftest start-ex
  (let [partial-failure (try
                          (clip/start
                            {:components
                             {:a {:start "2020"}
                              :b {:start `(+ (clip/ref :a) 20)}}})
                          (catch Throwable t t))]
    (is (reverse-contains? ::clip/system (ex-data partial-failure)))
    (is (reverse-contains? :a (::clip/system (ex-data partial-failure))))
    (is (not (reverse-contains? :b (::clip/system (ex-data partial-failure)))))))

(deftest stop
  (let [stopped (atom {})
        running-system {:foo 1 :bar 2}]
    (clip/stop {:components
                {:foo {:stop (list swap! stopped assoc :foo 'this)}
                 :bar {:stop (list swap! stopped assoc :bar 'this)}}}
               running-system)
    (is (= running-system @stopped)))

  (let [closed? (atom false)
        auto-closable (reify
                        java.io.Closeable
                        (close [this]
                          (reset! closed? true)))]
    (clip/stop {:components
                {:foo {}}}
               {:foo auto-closable})
    (is (= true @closed?))))

(deftest evaluation
  (are [x y] (= x (::foo (clip/start {:components {::foo {:start y}}})))
       2 (list inc 1)
       2 (list 'inc 1)
       2 (list `inc 1)
       2 (cons `inc [1])
       [0 1 2 3 4] '(range 5)
       [1 2 3 4 5] '(map inc (range 5))
       [1 2 3 4 5] `(map inc (range 5))
       [inc] [inc]
       ['inc] ['inc]
       [2] [(list inc 1)]
       {:foo 2} {:foo (list inc 1)}
       '{foo 2} {'foo (list inc 1)}
       '{foo 2} (edn/read-string (pr-str '{foo (inc 1)})))
  (are [x y z] (= x (::foo (clip/start {:components {::init {:start z
                                                             :resolve y}
                                                     ::foo {:start (clip/ref ::init)}}})))
       5 :foo {:foo 5}
       6 'inc 5
       6 inc 5)
  (are [x] (= (meta x)
              (meta (::foo (clip/start {:components {::foo {:start x}}}))))
       ^:foo [:a :b :c :d]
       ^:foo {:a :b :c :d}))
