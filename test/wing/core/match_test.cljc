(ns wing.core.match-test
  (:require [wing.core.match :as sut]
            #?(:clj [clojure.test :as t :refer [deftest is testing]]
               :cljs [cljs.test :as t :include-macros true :refer [deftest is testing]])))

(deftest match?-test
  (testing "returns whether the pattern matches"
    (is (sut/match? 5 5))
    (is (sut/match? _ 5))
    (is (= false (sut/match? {:name _} {:foo 5})))))
