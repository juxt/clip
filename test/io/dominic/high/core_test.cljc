(ns io.dominic.high.core-test
  (:require
    [clojure.test :refer [deftest is]]
    [io.dominic.high.core :as high]))

(deftest start
  (is (= {:foo 1 :bar 2}
         (high/start
           {:components
            {:foo {:start 1}
             :bar {:start '(inc (high/ref :foo))}}})))

  (is (= {nil 1 :bar 2}
         (high/start
           {:components
            {nil {:start 1}
             :bar {:start '(inc (high/ref nil))}}})))

  (is (= {:foo 1 :bar 3}
         (high/start
           {:components
            {:foo {:start 1
                   :resolve inc}
             :bar {:start '(inc (high/ref :foo))}}})))
  
  (is (= {:foo 1 :bar 3 :baz 4}
         (high/start
           {:components
            {:foo {:start 1
                   :resolve inc}
             :bar {:start '(inc (high/ref :foo))}
             :baz {:start (high/with-deps
                            [{:keys [foo bar]}]
                            (+ foo bar))}}}))))

(deftest start-graph-ex
  (is (thrown? Throwable
               (high/start
                 {:components
                  {:a {:start (high/ref :b)}
                   :b {:start (high/ref :a)}}})))
  (is (thrown? Throwable
               (high/start
                 {:components
                  {:a {:start (high/ref :b)}
                   :b {:start (high/ref :c)}
                   :c {:start (high/ref :a)}}})))
  (is (thrown? Throwable
               (high/start
                 {:components
                  {:a {:start (high/ref :b)}
                   :b {:start (high/ref :fourohfour)}}}))))

(defn- reverse-contains?
  "contains? but with arguments switched for better `is` compatibility."
  [key coll]
  (contains? coll key))

(deftest start-ex
  (let [partial-failure (try
                          (high/start
                            {:components
                             {:a {:start "2020"}
                              :b {:start `(+ (high/ref :a) 20)}}})
                          (catch Throwable t t))]
    (is (reverse-contains? ::high/system (ex-data partial-failure)))
    (is (reverse-contains? :a (::high/system (ex-data partial-failure))))
    (is (not (reverse-contains? :b (::high/system (ex-data partial-failure)))))))

(deftest stop
  (let [stopped (atom {})
        running-system {:foo 1 :bar 2}]
    (high/stop {:components
                {:foo {:stop (list swap! stopped assoc :foo 'this)}
                 :bar {:stop (list swap! stopped assoc :bar 'this)}}}
               running-system)
    (is (= running-system @stopped)))

  (let [closed? (atom false)
        auto-closable (reify
                        java.io.Closeable
                        (close [this]
                          (reset! closed? true)))]
    (high/stop {:components
                {:foo {}}}
               {:foo auto-closable})
    (is (= true @closed?))))
