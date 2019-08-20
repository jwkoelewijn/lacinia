(ns com.walmartlabs.lacinia.directives.schema
  (:require
    [clojure.test :refer [deftest is testing run-tests]]
    [com.walmartlabs.test-utils :refer [expect-exception]]
    [com.walmartlabs.lacinia.schema :as schema]
    [com.walmartlabs.test-schema :refer [test-schema]]
    [com.walmartlabs.lacinia :refer [execute]]))

(defmacro directive-test
  [expected-msg expected-ex-data schema]
  `(expect-exception ~expected-msg ~expected-ex-data (schema/compile ~schema)))

(deftest unknown-argument-type-for-directive
  (directive-test
    "Unknown argument type."
    {:arg-name :date
     :arg-type-name :Date
     :schema-types {:object [:MutationRoot
                             :QueryRoot
                             :SubscriptionRoot]
                    :scalar [:Boolean
                             :Float
                             :ID
                             :Int
                             :String]}}
    {:directive-defs
     {:Ebb {:locations #{:enum}
            :args {:date {:type :Date}}}}}))

(deftest incorrect-argument-type-for-directive
  (directive-test
    "Directive argument is not a scalar type."
    {:arg-name :user
     :arg-type-name :User
     :schema-types {:object [:MutationRoot
                             :QueryRoot
                             :SubscriptionRoot
                             :User]
                    :scalar [:Boolean
                             :Float
                             :ID
                             :Int
                             :String]}}
    {:directive-defs {:Ebb {:locations #{:enum}
                            :args {:user {:type :User}}}}
     :objects
     {:User
      {:fields
       {:name {:type :String}}}}}))

(deftest object-directive-unknown-type
  (directive-test
    "Object `User' references unknown directive @Unknown."
    {:directive-type :Unknown
     :object :User}
    {:objects
     {:User {:directives [{:directive-type :Unknown}]
             :fields {}}}}))

(deftest object-directive-inapplicable
  (directive-test
    "Directive @Ebb on object `Flow' is not applicable."
    {:allowed-locations #{:enum}
     :directive-type :Ebb
     :object :Flow}
    {:directive-defs
     {:Ebb {:locations #{:enum}}}
     :objects
     {:Flow {:fields {}
             :directives [{:directive-type :Ebb}]}}}))

(deftest union-directive-unknown-type
  (directive-test
    "Union `Ebb' references unknown directive @Unknown."
    {:directive-type :Unknown
     :union :Ebb}
    {:objects
     {:Flow {:fields {}}}
     :unions
     {:Ebb {:members [:Flow]
            :directives [{:directive-type :Unknown}]}}}))

(deftest union-directive-inapplicable
  (directive-test
    "Directive @deprecated on union `Ebb' is not applicable."
    {:allowed-locations #{:enum-value
                          :field-definition}
     :directive-type :deprecated
     :union :Ebb}
    {:objects
     {:Flow {:fields {}}}
     :unions
     {:Ebb {:members [:Flow]
            ;; Can only deprecate fields and enum values
            :directives [{:directive-type :deprecated}]}}}))

(def ^:private mock-conformer identity)

(deftest scalar-directive-unknown-type
  (directive-test
    "Scalar `Ebb' references unknown directive @Unknown."
    {:directive-type :Unknown
     :scalar :Ebb}
    {:scalars
     {:Ebb {:parse mock-conformer
            :serialize mock-conformer
            :directives [{:directive-type :Unknown}]}}}))

(deftest scalar-directive-inapplicable
  (directive-test
    "Directive @deprecated on scalar `Ebb' is not applicable."
    {:allowed-locations #{:enum-value
                          :field-definition}
     :directive-type :deprecated
     :scalar :Ebb}
    {:scalars
     {:Ebb {:parse mock-conformer
            :serialize mock-conformer
            ;; Can only deprecate fields and enum values
            :directives [{:directive-type :deprecated}]}}}))

(deftest field-directive-unknown-type
  (directive-test
    "Field `User/id' references unknown directive @Unknown."
    {:directive-type :Unknown
     :field-name :User/id}
    {:objects
     {:User
      {:fields {:id {:type :String
                     :directives [{:directive-type :Unknown}]}}}}}))

(deftest field-directive-inapplicable
  (directive-test
    "Directive @Ebb on field `Flow/id' is not applicable."
    {:allowed-locations #{:enum}
     :directive-type :Ebb
     :field-name :Flow/id}
    {:directive-defs
     {:Ebb {:locations #{:enum}}}
     :objects
     {:Flow
      {:fields
       {:id {:type :String
             :directives [{:directive-type :Ebb}]}}}}}))

(deftest argument-directive-unknown-type
  (directive-test
    "Argument `User/id.ebb' references unknown directive @Unknown."
    {:arg-name :User/id.ebb
     :directive-type :Unknown}
    {:objects
     {:User
      {:fields
       {:id {:type :String
             :args {:ebb {:type :String
                          :directives [{:directive-type :Unknown}]}}}}}}}))

(deftest argument-directive-inapplicable
  (directive-test
    "Directive @Ebb on argument `Flow/id.format' is not applicable."
    {:allowed-locations #{:enum}
     :arg-name :Flow/id.format
     :directive-type :Ebb}
    {:directive-defs
     {:Ebb {:locations #{:enum}}}
     :objects
     {:Flow
      {:fields
       {:id {:type :String
             :args
             {:format {:type :String
                       :directives [{:directive-type :Ebb}]}}}}}}}))

(deftest enum-directive-unknown-type
  (directive-test
    "Enum `Colors' references unknown directive @Unknown."
    {:directive-type :Unknown
     :enum :Colors}
    {:enums
     {:Colors
      {:values [:red :green :blue]
       :directives [{:directive-type :Unknown}]}}}))

(deftest enum-directive-inapplicable
  (directive-test
    "Directive @nope on enum `Colors' is not applicable."
    {:allowed-locations #{:object}
     :directive-type :nope
     :enum :Colors}
    {:directive-defs {:nope {:locations #{:object}}}
     :enums
     {:Colors
      {:values [:red :green :blue]
       :directives [{:directive-type :nope}]}}}))

(deftest enum-value-directive-unknown-type
  (directive-test
    "Enum value `Colors/red' referenced unknown directive @Unknown."
    {:directive-type :Unknown
     :enum-value :Colors/red}
    {:enums
     {:Colors
      {:values [{:enum-value :red
                 :directives [{:directive-type :Unknown}]}
                :green :blue]}}}))

(deftest can-deprecate-object-fields
  (let [schema (schema/compile
                 {:objects
                  {:User
                   {:fields {:id {:type :ID
                                  :directives [{:directive-type :deprecated}]}
                             :name {:type :String
                                    :directives [{:directive-type :deprecated
                                                  :directive-args
                                                  {:reason "Use full_name instead."}}]}}}}})]
    (is (= true
           (get-in schema [:User :fields :id :deprecated])))
    (is (= "Use full_name instead."
           (get-in schema [:User :fields :name :deprecated])))))

(deftest can-deprecate-interface-fields
  (let [schema (schema/compile
                 {:interfaces
                  {:Account
                   {:fields {:id {:type :ID
                                  :directives [{:directive-type :deprecated}]}}}}})]
    (is (= true
           (get-in schema [:Account :fields :id :deprecated])))))


(deftest can-deprecate-enum-values
  (let [schema (schema/compile
                 {:enums
                  {:Color
                   {:values [:red
                             {:enum-value :green
                              :directives [{:directive-type :deprecated}]}]}}})]
    (is (= {:green {:directives [{:directive-type :deprecated}]
                    :deprecated true
                    :enum-value :green}
            :red {:enum-value :red}}
           (get-in schema [:Color :values-detail])))))

(comment
 (run-tests))
