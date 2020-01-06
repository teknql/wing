(ns wing.core.walk-test
  (:require [wing.core.walk :as sut]
            #?(:clj [clojure.test :as t :refer [deftest testing is]]
               :cljs [cljs.test :as t :include-macros true :refer [deftest testing is]])
            [clojure.walk :as walk]))

(deftest pathwalk-pre-test
  (testing "calls in the same order as prewalk, minus map-keys + raw map-entries"
    (let [prewalk-calls       (transient [])
          sut-calls           (transient [])
          transform           #(if (int? %)
                                 (inc %)
                                 %)
          record-prewalk-call #(do (when-not (or (map-entry? %) (keyword? %))
                                     (conj! prewalk-calls %))
                                   (transform %))
          record-sut-calls    #(do (conj! sut-calls %2)
                                   (transform %2))
          data                {:a 5 :b 6 :c {:d true
                                             :e [{:foo nil}]}}
          walk-result         (walk/prewalk record-prewalk-call data)
          sut-result          (sut/pathwalk-pre record-sut-calls data)
          prewalk-calls       (persistent! prewalk-calls)
          sut-calls           (persistent! sut-calls)]
      (is (= prewalk-calls
             sut-calls))
      (is (= walk-result sut-result)))))

(deftest pathwalk-post-test
  (testing "calls in the same order as postwalk, minus map-keys + raw map-entries"
    (let [postwalk-calls       (transient [])
          sut-calls            (transient [])
          transform            #(if (int? %)
                                  (inc %)
                                  %)
          record-postwalk-call #(do (when-not (or (map-entry? %) (keyword? %))
                                      (conj! postwalk-calls %))
                                    (transform %))
          record-sut-calls     #(do (conj! sut-calls %2)
                                    (transform %2))
          data                 {:a 5 :b 6 :c {:d true
                                              :e [{:foo nil}]}}
          walk-result          (walk/postwalk record-postwalk-call data)
          sut-result           (sut/pathwalk-post record-sut-calls data)
          postwalk-calls       (persistent! postwalk-calls)
          sut-calls            (persistent! sut-calls)]
      (is (= postwalk-calls
             sut-calls))
      (is (= walk-result sut-result)))))
