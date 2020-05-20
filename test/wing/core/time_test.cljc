(ns wing.core.time-test
  (:require [wing.core.time :as sut]
            [clojure.test :refer [deftest testing is]]
            [tick.alpha.api :as t]))


(deftest divisible?-test
  (testing "returns true for durations that are evenly divisible"
    (is (sut/divisible? (t/new-duration 20 :minutes) (t/new-duration 1 :minutes)))
    (is (sut/divisible? (t/new-duration 20 :minutes) (t/new-duration 5 :minutes))))
  (testing "returns false for durations that are not evenly divisible"
    (is (not (sut/divisible? (t/new-duration 19 :minutes) (t/new-duration 5 :minutes))))))

(deftest round-to-test
  (testing "rounds down to a divisible value"
    (is (= (t/at (t/new-date 2018 1 1) (t/new-time 1 5))
           (sut/round-to (t/at (t/new-date 2018 1 1) (t/new-time 1 7))
                         (t/new-duration 5 :minutes))))))

(deftest ensure-chronological-test
  (let [times (take 3 (iterate #(t/>> % (t/new-duration 1 :minutes)) (t/now)))]
    (testing "filters non-chronological items"
      (is (= times
             (sut/ensure-chronological times)))
      (is (= (list (last times))
             (sut/ensure-chronological (reverse times)))))

    (testing "using an accessor"
      (let [people (map #(array-map :name "Bob" :birth-day %) times)]
        (is (= times
               (map :birth-day (sut/ensure-chronological :birth-day people))))))))
