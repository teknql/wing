(ns wing.core.resource-test
  (:require [wing.core.resource :as sut :refer [with-resource Resource]]
            [clojure.test :as t :refer [deftest testing is]]))



(deftest with-resource-test
  (testing "calls release in reverse order"
    (let [release-calls (atom 0)]
      (with-resource [foo (reify Resource
                            (release [x]
                              (is (= 1 @release-calls))
                              (swap! release-calls inc)))
                      bar (reify Resource
                            (release [x]
                              (is (= 0 @release-calls))
                              (swap! release-calls inc)))])
      (is (= 2 @release-calls))))

  (testing "returns the result of the expression"
    (let [result
          (with-resource [foo (reify Resource
                                (release [x] nil))]
            5)]
      (is (= 5 result))))

  (testing "calls release even if there is an error, and propagates it"
    (let [release-called (atom false)]
      (is (thrown?
            Exception
            (with-resource [foo (reify Resource
                                  (release [x] (reset! release-called true)))]
              (throw (Exception. "Boom!")))))
      (is @release-called))))
