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
      (is (= {:name     "Edward"
              :age      42
              :pet-name "Samson"}
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
      (is (= {:person-name "Edward"
              :person-age  42
              :name        "Samson"}
             encoded))
      (is (= val decoded)))))
