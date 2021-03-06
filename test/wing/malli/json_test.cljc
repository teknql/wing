(ns wing.malli.json-test
  (:require [wing.malli.json :as sut]
            #?(:clj [clojure.test :as t :refer [deftest testing is]]
               :cljs [cljs.test :as t :refer [deftest testing is] :include-macros true])
            [malli.core :as m]
            [malli.generator :as mg]
            [cuerdas.core :as str]
            [wing.malli :as wm]))


(def person-schema
  [:map
   [:person/name string?]
   [:person/age pos-int?]
   [:person/eye-color (wm/enum-keyword #{:color/blue :color/green :color/blue-green})]
   [:person/nickname {:json/key "alias"} string?]
   [:person.personality/charisma pos-int?]
   [:pet/name string?]])

(deftest encoder+decoder-test
  (let [person->json (m/encoder person-schema (sut/transformer))
        json->person (m/decoder person-schema (sut/transformer))
        data         (mg/generate person-schema)]
    (testing "round-trip"
      (is (= data
             (-> data person->json json->person))))

    (testing "decoding maps"
      (is (= {:person/name                 "Bob"
              :person/age                  35
              :person/nickname             "Robert"
              :person/eye-color            :color/blue-green
              :person.personality/charisma 50
              :pet/name                    "Rupert"}
             (json->person {"name"                 "Bob"
                            "alias"                "Robert"
                            "age"                  35
                            "personality_charisma" 50
                            "eye_color"            "blue_green"
                            "pet_name"             "Rupert"}))))

    (testing "custom string functions"
      (let [opts         {:json/encode-map-key str/camel
                          :json/encode-enum    str/camel}
            transformer  (sut/transformer)
            person->json (m/encoder person-schema opts transformer)
            json->person (m/decoder person-schema opts transformer)
            data         (assoc data :person/eye-color :color/blue-green)
            encoded      (person->json data)
            decoded      (json->person encoded)]
        (is (= data decoded))
        (is (contains? encoded "petName"))
        (is (= "blueGreen" (get encoded "eyeColor")))))))
