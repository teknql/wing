(ns wing.core-test
  (:require [wing.core :as sut :refer [fex] :include-macros true]
            [clojure.test :refer [deftest testing is are]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))


(def gen-simple-map
  "Generator for simple map"
  (let [simple-gen (gen/one-of [gen/keyword gen/int gen/string gen/boolean])
        simple-val (gen/frequency [[5 simple-gen]
                                   [1 (gen/map simple-gen simple-gen)]
                                   [1 (gen/list simple-gen)]
                                   [1 (gen/vector simple-gen)]
                                   [1 (gen/set simple-gen)]])]
    (gen/map simple-gen simple-val)))

(deftest deep-merge-test
  (testing "is identity for single arguments"
    (is (= 3 (sut/deep-merge 3)))
    (is (= {:a 1 :b 2} (sut/deep-merge {:a 1 :b 2}))))

  (testing "behaves like merge for unnested maps"
    (let [a {:a 1}
          b {:b 2}
          c {:a 3 :c 4}]
      (is (= (merge a b c)
             (sut/deep-merge a b c)))))

  (testing "supports merging deeply nested maps"
    (let [a {:a 1 :b {:x 2 :y 3}}
          b {:c 4 :d {:z 5}}
          c {:a 6 :b {:x 7 :z 8}}]
      (is (= {:a 6
              :b {:x 7 :y 3 :z 8}
              :c 4
              :d {:z 5}}
             (sut/deep-merge a b c))))))

(defspec apply-if-is-identity-if-pred-returns-false
  5
  (prop/for-all [x gen/any-equatable]
      (is (= x
             (sut/apply-if x (constantly false) (constantly :modified))))))

(defspec apply-if-applies-if-pred-returns-true
  5
  (prop/for-all [x gen/int]
      (is (= (inc x)
             (sut/apply-if x (constantly true) inc)))))

(defspec apply-when-is-nil-if-pred-returns-false
  5
  (prop/for-all [x gen/any-equatable]
      (is (nil?
            (sut/apply-when x (constantly false) (constantly :modified))))))

(defspec apply-when-applies-if-pred-returns-true
  5
  (prop/for-all [x gen/int]
      (is (= (inc x)
             (sut/apply-when x (constantly true) inc)))))

(defspec guard-returns-nil-if-pred-is-false
  5
  (prop/for-all [x gen/any-equatable]
      (is (nil? (sut/guard (constantly false) x)))))

(defspec guard-is-identity-if-pred-is-true
  5
  (prop/for-all [x gen/any-equatable]
      (is (= x (sut/guard (constantly true) x)))))

(deftest update-if-exists-test
  (testing "updates existing keys"
    (is (= {:a 2 :b 1}
           (sut/update-if-exists {:a 1 :b 2}
                                 :a inc
                                 :b dec))))
  (testing "does nothing for missing keys"
    (is (= {:a 1}
           (sut/update-if-exists {:a 1} :b inc))))
  #?(:clj
     (testing "raises an argument error if needed"
       (is (thrown? IllegalArgumentException
                    (sut/update-if-exists {} :b))))))

(deftest update-if-some-test
  (testing "updates existing keys"
    (is (= {:a 2 :b 1}
           (sut/update-if-some {:a 1 :b 2}
                               :a inc
                               :b dec))))
  (testing "does nothing for missing keys"
    (is (= {:a 1 :b nil}
           (sut/update-if-some {:a 1
                                :b nil} :b inc))))
  (testing "does nothing for missing keys"
    (is (= {:a 1}
           (sut/update-if-exists {:a 1} :b inc))))
  #?(:clj
     (testing "raises an argument error if needed"
       (is (thrown? IllegalArgumentException
                    (sut/update-if-some {} :b))))))

(defspec partition-keys-all-keys-first-is-identity
  100
  (prop/for-all [m gen-simple-map]
      (= m
         (first (sut/partition-keys m (keys m))))))

(defspec partition-keys-all-keys-second-is-empty
  100
  (prop/for-all [m gen-simple-map]
      (= {}
         (second (sut/partition-keys m (keys m))))))

(defspec partition-keys-no-keys-first-is-empty
  100
  (prop/for-all [m gen-simple-map]
      (= {} (first (sut/partition-keys m [])))))

(defspec partition-keys-no-keys-second-is-identity
  100
  (prop/for-all [m gen-simple-map]
      (= m (second (sut/partition-keys m [])))))


(deftest binary-partition-test
  (is (= [[1 3 5] [2 4 6]]
         (sut/binary-partition odd? [1 2 3 4 5 6]))))

(deftest extract-test
  (is (= [[42 200] {:name "Bob"}]
         (sut/extract {:name "Bob" :weight 200 :age 42}
                      [:age :weight]))))


(deftest sum-test
  (testing "sums the numbers"
    (is 6 (sut/sum 1 2 3)))
  (testing "handles nil values"
    (is 3 (sut/sum 1 2 nil))))

(deftest avg-test
  (testing "returns the average of the numbers"
    (is 2 (sut/avg 1 2 3)))

  (testing "returns nil when called with empty args"
    (is (nil? (sut/avg)))))

(deftest standard-deviation-test
  (testing "returns nil for empty sequences"
    (is (nil? (sut/standard-deviation))))

  (testing "returns 0 for all the same value"
    (is (= 0.0 (apply sut/standard-deviation (repeat 10 5)))))

  (testing "returns the currect result"
    (is (= 3.94 (sut/round (sut/standard-deviation  4 9 11 12 17 5 8 12 14) 2)))))


(deftest round-test
  (testing "obeys precision"
    (are [expected n precision] (= expected (sut/round n precision))
      1.0   1.1234567 0
      1.1   1.1234567 1
      1.12  1.1234567 2
      1.123 1.1234567 3)))

(deftest find-first-test
  (testing "returns items based on pred"
    (is (= 2 (sut/find-first even? [1 2 3 4]))))
  (testing "applies extract if provided"
    (is (= 3 (sut/find-first even? inc [1 2 4])))))

(deftest find-ix-test
  (testing "returns the index"
    (is (= 0 (sut/find-first-ix even? [2 3])))
    (is (= nil (sut/find-first-ix #{5} [2 3])))))

(deftest indistinct-test
  (let [input [1 2 3 4 5 6 1 2 3 7 8 9]]
    (testing "works over a collection"
      (is (= [1 2 3]
             (->> input
                  (sut/indistinct)
                  (into [])))))
    (testing "works as a transducer"
      (is (= [1 2 3]
             (into [] (sut/indistinct) input))))))

(defspec rand-is-bounded-prop
  100
  (prop/for-all
      [x (gen/double* {:min 0 :max 1 :NaN? false :infinite? false})
       y (gen/double* {:min 1 :max 2 :NaN? false :infinite? false})]
      (<= x (sut/rand x y) y)))

(defspec rand-is-random-prop
  100
  (prop/for-all [x (gen/such-that #(not= 0.0 %) (gen/double* {:infinite? false
                                                              :NaN?      false}))]
      (not= (sut/rand x) (sut/rand x))))


(deftest inspect-test
  (testing "returns itself"
    ;; Capture stdout to avoid poluting the console
    (with-out-str
      (is (= 5 (sut/inspect 5)))
      (is (= 5 (sut/inspect "value" 5)))
      (is (= 5 (sut/inspect "value" inc 5)))))

  (testing "prints the correct output"
    (is (= "5\n" (with-out-str (sut/inspect 5))))
    (is (= "Value: 5\n" (with-out-str (sut/inspect "Value" 5))))
    (is (= "Value: 6\n" (with-out-str (sut/inspect "Value" inc 5))))))


(deftest arg-test
  (testing "returns the correct argument"
    (is (= 0 ((sut/arg 1) 0 1 2 3)))
    (is (= 1 ((sut/arg 2) 0 1 2 3)))
    (is (= 2 ((sut/arg 3) 0 1 2 3)))
    (is (= 3 ((sut/arg 4) 0 1 2 3)))
    (is (= nil ((sut/arg 5) 0 1 2 3)))))


(deftest dedupe-by-test
  (testing "removes consecutive elements"
    (let [coll [{:name "Bob" :age 31}
                {:name "Ed" :age 31}
                {:name "Stacy" :age 35}
                {:name "Nemo" :age 42}
                {:name "Stacy" :age 35}]]
      (is (= ["Bob" "Stacy" "Nemo" "Stacy"]
             (->> coll
                  (sut/dedupe-by :age)
                  (map :name)
                  (into [])))))))

(deftest distinct-by-test
  (testing "removes non-unique elements"
    (let [coll [{:name "Bob" :age 31}
                {:name "Ed" :age 31}
                {:name "Stacy" :age 35}
                {:name "Nemo" :age 42}
                {:name "Stacy" :age 35}]]
      (is (= ["Bob" "Stacy" "Nemo"]
             (->> coll
                  (sut/distinct-by :age)
                  (map :name)
                  (into [])))))))

(deftest ensure-ascending-test
  (is (= [1 2 3 4 5]
         (sut/ensure-ascending [1 1 0 2 1 3 4 3 2 1 5]))))

(deftest map-keys-test
  (testing "maps f over all keys"
    (is (= {1 :a
            2 :b
            3 :c}
           (sut/map-keys inc {0 :a
                              1 :b
                              2 :c}))))
  (testing "nil punning"
    (is (nil? (sut/map-keys inc nil))))

  (testing "meta data preservation"
    (is (= {:meta true}
           (meta
             (sut/map-keys str (with-meta {:foo 5} {:meta true})))))))

(deftest map-vals-test
  (testing "maps f over all values"
    (is (= {:a 1
            :b 2
            :c 3}
           (sut/map-vals inc {:a 0
                              :b 1
                              :c 2}))))

  (testing "nil punning"
    (is (nil? (sut/map-vals inc nil))))

  (testing "meta data preservation"
    (is (= {:meta true}
           (meta
             (sut/map-vals inc (with-meta {:foo 5} {:meta true})))))))

(deftest map-leaves-test
  (testing "maps f over all leaves"
    (is (= {:a 1
            :b 2
            :c {:a 1}}
           (sut/map-leaves inc {:a 0
                                :b 1
                                :c {:a 0}}))))

  (testing "nil punning"
    (is (nil? (sut/map-leaves inc nil))))

  (testing "meta data preservation"
    (is (= {:meta true}
           (meta
             (sut/map-leaves inc (with-meta {:foo 5} {:meta true})))))))

(deftest remove-vals-test
  (testing "removes all values for which f returns true"
    (is (= {:b 1
            :c 2}
           (sut/remove-vals nil? {:a nil
                                  :b 1
                                  :c 2}))))

  (testing "nil punning"
    (is (nil? (sut/remove-vals nil? nil))))

  (testing "meta data preservation"
    (is (= {:meta true}
           (meta
             (sut/remove-vals pos? (with-meta {:foo 5} {:meta true})))))))

(deftest filter-val-test
  (testing "removes all values for which f returns false"
    (is (= {:b 1
            :c 2}
           (sut/filter-vals some? {:a nil
                                   :b 1
                                   :c 2}))))

  (testing "nil punning"
    (is (nil? (sut/filter-vals nil? nil))))

  (testing "meta data preservation"
    (is (= {:meta true}
           (meta
             (sut/filter-vals pos? (with-meta {:foo 5} {:meta true})))))))

(deftest remove-keys-test
  (testing "removes all values for which f returns true"
    (is (= {:b 1
            :c 2}
           (sut/remove-keys #{:a} {:a nil
                                   :b 1
                                   :c 2}))))

  (testing "nil punning"
    (is (nil? (sut/remove-keys nil? nil))))

  (testing "meta data preservation"
    (is (= {:meta true}
           (meta
             (sut/remove-keys string? (with-meta {:foo 5} {:meta true})))))))

(deftest filter-keys-test
  (testing "removes all values for which f returns false"
    (is (= {:b 1
            :c 2}
           (sut/filter-keys #{:b :c} {:a nil
                                      :b 1
                                      :c 2}))))

  (testing "nil punning"
    (is (nil? (sut/filter-keys nil? nil))))

  (testing "meta data preservation"
    (is (= {:meta true}
           (meta
             (sut/filter-keys string? (with-meta {:foo 5} {:meta true})))))))


(deftest ns-keys-test
  (testing "applies a namespace to non-namespaces keys"
    (is (= {:test/foo "A"
            :test/bar "B"}
           (sut/ns-keys "test"
                        {:foo "A"
                         :bar "B"}))))

  (testing "removes a namespace with `nil` as the namespace"
    (is (= {:foo "A" :bar "B" :baz "C"}
           (sut/ns-keys nil {:test/foo "A"
                             :other-test/bar "B"
                             :baz "C"}))))

  (testing "nil punning"
    (is (nil? (sut/ns-keys "foo" nil))))

  (testing "meta data preservation"
    (is (= {:meta true}
           (meta
             (sut/ns-keys "foo" (with-meta {:foo 5} {:meta true})))))))


(deftest assoc-some
  (testing "is identity for nil vals"
    (let [m {:a 1 :b 2 :c 3}]
      (is (= m
             (sut/assoc-some m :a nil :d nil)))))

  (testing "is assoc for non-nil vals"
    (let [m {:a 1 :b 2 :c 3}]
      (is (= {:a 2 :b 2 :c 3}
             (sut/assoc-some m :a 2 :c nil))))))


(deftest fex-test
  (testing "returns the val of f if nothing is thrown"
    (is (= 5
           ((sut/fex identity :fail) 5))))

  (testing "returns the exceptional-val if something is thrown"
    (is (= :fail
           ((sut/fex (fn [_] (throw (ex-info "Fail" {}))) :fail)
            5)))))

(deftest find-paths-test
  (testing "non-nested maps"
    (is (= '([:a]
             [:c])
           (sut/find-paths (fex even? false) {:a 2 :b 3 :c 4}))))
  (testing "nested maps"
    (is (= '([:a]
             [:b :c]
             [:b :e :f])
           (sut/find-paths (fex even? false) {:a 2
                                              :b {:c 4 :d 5 :e {:f 6}}
                                              :c 3}))))

  (testing "nested vectors"
    (is (= '([:a 0 0]
             [:b :c 0 :d])
           (sut/find-paths (fex even? false)
                           {:a [[2] [3 [5]]]
                            :b {:c [{:d 4}]}}))))
  (testing "nested vectors target nested maps"
    (is (= '([:b :c 0])
           (sut/find-paths (fex #(contains? % :d) false)
                           {:a [[2] [3 [5]]]
                            :b {:c [{:d 4}]}})))))


(deftest dissoc-in-test
  (testing "is identical to dissoc for single path"
    (let [some-map (gen/generate gen-simple-map)
          tgt-key  (key (rand-nth (into [] identity some-map)))]
      (is (= (dissoc some-map tgt-key)
             (sut/dissoc-in some-map [tgt-key])))))

  (testing "supports dissocing in nested paths"
    (let [m {:a 1 :b 2 :c 3 :d {:e {:f 6
                                    :g true}
                                :h 9}}]
      (is (= {:a 1 :b 2 :c 3 :d {:e {:g true}
                                 :h 9}}
             (sut/dissoc-in m [:d :e :f]))))))

(deftest index-by-test
  (let [data [{:name "Bob" :age 42}
              {:name "Alice" :age 63}
              {:name "Steve" :age 6}
              {:name "Edward" :age 22}
              {:name "Maxine" :age 26}]]
    (testing "uses `f` to build an index"
      (let [name->rec (sut/index-by :name data)]
        (is (= (count data)
               (count name->rec)))

        (doseq [{:keys [name] :as rec} data]
          (is (= rec (name->rec name))))))

    (testing "optionally takes a `g` to build the value"
      (let [name->age (sut/index-by :name :age data)]

        (is (= (count data)
               (count name->age)))

        (doseq [{:keys [name age]} data]
          (is (= age(name->age name))))))))

(deftest group-by-test
  (let [data [{:name "Bob" :age 31}
              {:name "Alice" :age 31}
              {:name "Steve" :age 25}
              {:name "Maxine" :age 25}]]
    (testing "behaves like clojure.core/group-by for simple calls"
      (is (= (group-by :age data)
             (sut/group-by :age data))))

    (testing "allows providing a view function"
      (is (= {31 ["Bob" "Alice"]
              25 ["Steve" "Maxine"]}
             (sut/group-by :age :name data))))

    (testing "allows specifying an into collection type"
      (is (= {31 #{"Bob" "Alice"}
              25 #{"Steve" "Maxine"}}
             (sut/group-by :age :name #{} data))))))

(deftest unfold-test
  (testing "skips nil items, and terminates on nil"
    (let [f #(cond
               (even? %) [nil (inc %)]
               (= 7 %)   nil
               :else     [% (inc %)])]
      (is (= '(1
                3
                5)
             (sut/unfold f 1))))))

(deftest arity-test
  (let [f (fn [& args]
            args)]
    (testing "only calls f with the appropriate number of args"
      (is (= [1]
             ((sut/arity 1 f) 1 2 3)))
      (is (= [1 2]
             ((sut/arity 2 f) 1 2 3))))
    (testing "static arg application"
      (is (= [1 2 4]
             ((sut/arity 2 f 4) 1 2 3))))))

(deftest make-map-test
  (let [a 1
        b 2]
    (testing "just syms"
      (is (= {:a 1 :b 2}
             (sut/make-map a b))))
    (testing "and keywords"
      (is (= {:a 1 :b 2 :c 3}
             (sut/make-map a :b b :c 3))))
    (testing "and strings"
      (is (= {"a" 1 :b 2}
             (sut/make-map "a" a b))))))

(deftest random-uuid-test
  (is (uuid? (sut/random-uuid))))

(deftest compr-test
  (testing "is just like comp for arity one fns"
    (let [fns       [inc inc inc]
          via-compr (sut/compr fns)
          via-comp  (comp fns)]
      (is (= (via-compr 1) (via-comp 1)))))
  (testing "works with multi-arity fns"
    (let [f (sut/compr
              #(update %1 :sum + %2)
              #(update %1 :product * %2))]
      (is (= {:sum 10 :product 24}
             (reduce f {:sum 0 :product 1} [1 2 3 4]))))))

(deftest over-test
  (is (= (+ 2 3 4 5)
         ((sut/over + inc) 1 2 3 4))))
