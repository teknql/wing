(ns wing.malli-test
  (:require [wing.malli :as sut]
            #?(:clj [clojure.test :refer [deftest testing is]]
               :cljs [cljs.test :as t :include-macros true :refer [deftest testing is]])
            [malli.core :as m]))


(deftest enum-keyword-test
  (testing "encode/decode"
    (let [schema  (sut/enum-keyword [:color/red :color/green])
          val     :color/red
          encoded (m/encode schema val (sut/json-transformer))
          decoded (m/decode schema encoded (sut/json-transformer))]
      (is (= "red" encoded))
      (is (= val decoded)))))


(deftest json-transformer-test
  (testing "encoding / decoding namespaced map"
    (let [schema  [:map
                   [:person/name string?]
                   [:person/age pos-int?]
                   [:pet/name string?]]
          val     {:person/name "Edward"
                   :person/age  42
                   :pet/name    "Samson"}
          encoded (m/encode schema val (sut/json-transformer))
          decoded (m/decode schema encoded (sut/json-transformer))]
      (is (= {"name"     "Edward"
              "age"      42
              "pet_name" "Samson"}
             encoded))
      (is (= val decoded))))

  (testing "encoding / decoding with explicit JSON namespace"
    (let [schema  [:map {:json/root-namespace "pet"}
                   [:person/name string?]
                   [:person/age pos-int?]
                   [:pet/name string?]]
          val     {:person/name "Edward"
                   :person/age  42
                   :pet/name    "Samson"}
          encoded (m/encode schema val (sut/json-transformer))
          decoded (m/decode schema encoded (sut/json-transformer))]
      (is (= {"person_name" "Edward"
              "person_age"  42
              "name"        "Samson"}
             encoded))
      (is (= val decoded)))))


(deftest default-fn-transformer-test
  (testing "calls :default-fn if the key is not present in the map"
    (is (= {:foo 5}
           (m/decode [:map [:foo {:default-fn (constantly 5)} int?]]
                     {}
                     (sut/default-fn-transformer))))))

(deftest empty-string-as-nil-transformer-test
  (testing "converts empty strings to nil"
    (is (= {:foo nil}
           (m/decode
             [:map [:foo [:maybe :string]]]
             {:foo ""}
             (sut/empty-string-as-nil-transformer))))))

(deftest strip-nil-keys-transformer-test
  (testing "strips nil keys from the map"
    (is (= {:foo 1
            :bar 2}
           (m/decode
             [:map
              [:foo int?]
              [:bar int?]
              [:baz {:optional true} boolean?]]
             {:foo 1 :bar 2 :baz nil}
             (sut/strip-nil-keys-transformer)))))
  (testing "allows falsey values into the map"
    (is (= {:foo 1
            :bar 2
            :baz false}
           (m/decode
             [:map
              [:foo int?]
              [:bar int?]
              [:baz {:optional true} boolean?]]
             {:foo 1 :bar 2 :baz false}
             (sut/strip-nil-keys-transformer))))))
