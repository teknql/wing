(ns wing.core.time-test
  (:require [wing.core.time :as sut]
            [clojure.test :refer [deftest testing is]]
            #?(:clj [java-time :as time]
               :cljs [cljs-time.core :as time])))


(deftest divide-test
  (is (= 5 (sut/divide (time/minutes 5) (time/minutes 1)))))

(deftest divisible?-test
  (testing "returns true for durations that are evenly divisible"
    (is (sut/divisible? (time/minutes 20) (time/minutes 1)))
    (is (sut/divisible? (time/minutes 20) (time/minutes 5))))
  (testing "returns false for durations that are not evenly divisible"
    (is (not (sut/divisible? (time/minutes 19) (time/minutes 5))))))

#?(:clj
   (deftest round-to-test
     (testing "rounds down to a divisible value"
       (is (= (time/zoned-date-time 2018 1 1 1 5)
              (sut/round-to (time/zoned-date-time 2018 1 1 1 7)
                            (time/minutes 5)))))))
