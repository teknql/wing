(ns wing.core.match-test
  (:require [wing.core.match :as sut #?(:clj :refer :cljs :refer-macros) [match?]]
            #?(:clj [clojure.test :as t :refer [deftest is testing]]
               :cljs [cljs.test :as t :refer-macros [deftest is testing]])))

(deftest match?-test
  (testing "returns whether the pattern matches"
    (is (match? 5 5))
    (is (match? _ 5))
    (is (= false (match? {:name _} {:foo 5})))))
