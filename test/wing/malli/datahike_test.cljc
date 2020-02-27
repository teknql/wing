(ns wing.malli.datahike-test
  (:require [wing.malli.datahike :as sut]
            #?(:clj [clojure.test :as t :refer [deftest testing is]]
               :cljs [cljs.test :as t :include-macros true
                      :refer-macros [deftest testing is]])))


(deftest ->datahike-schema-test
  (testing "simple map"
    (is (= [{:db/ident       :person/name
             :db/valueType   :db.type/string
             :db/cardinality :db.cardinality/one}
            {:db/ident       :person/age
             :db/valueType   :db.type/long
             :db/cardinality :db.cardinality/one}]
           (sut/->datahike-schema
             [:map
              [:person/name string?]
              [:person/age int?]]))))

  (testing "map with sequence-like value"
    (is (= [{:db/ident       :person/names
             :db/valueType   :db.type/string
             :db/cardinality :db.cardinality/many}]
           (sut/->datahike-schema
             [:map [:person/names [:vector string?]]]))))

  (testing "map with nested fields"
    (is (= [{:db/ident       :person/name
             :db/valueType   :db.type/string
             :db/cardinality :db.cardinality/one}
            {:db/ident       :person/location
             :db/valueType   :db.type/ref
             :db/cardinality :db.cardinality/one}
            {:db/ident       :location/city
             :db/valueType   :db.type/string
             :db/cardinality :db.cardinality/one}
            {:db/ident       :location/state
             :db/valueType   :db.type/string
             :db/cardinality :db.cardinality/one}]
           (sut/->datahike-schema
             [:map
              [:person/name string?]
              [:person/location
               [:map [:location/city string?] [:location/state string?]]]]))))

  (testing "associating properties from map-props and child schemas"
    (let [overrides {:db/ident       :foo/bar
                     :db/valueType   :db.type/boolean
                     :db/cardinality :db.cardinality/one
                     :db/index       true
                     :db/unique      :db.unique/identity}]
      (is (= [overrides]
             (sut/->datahike-schema
               [:map [:person/name overrides string?]])))
      (is (= [overrides]
             (sut/->datahike-schema
               [:map [:person/name [string? overrides]]]))))))
