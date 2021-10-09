(ns juxt.clip.edn-test
  (:require
    [clojure.test :refer [deftest is are]]
    [juxt.clip.edn :as clip.edn]))

(defn foo [])

(deftest analyze-test
  (is (= {:components {}}
         (clip.edn/analyze
           {:components {}})))
  (is (= {:components {}
          :executor #'foo}
         (clip.edn/analyze
           {:components {}
            :executor `foo}))))
