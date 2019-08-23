(ns com.walmartlabs.lacinia.directives.visitors-test
  (:require [clojure.test :refer :all]
            [com.walmartlabs.lacinia :refer [execute]]
            [com.walmartlabs.lacinia.schema :as schema]
            [com.walmartlabs.test-schema :refer [test-schema] :as ts]))


(def visitor-record (atom []))

(defmulti foo-visitor :category)

(defmethod foo-visitor :default [{:keys [category resolver execution-context field-selection]}]
  (swap! visitor-record #(conj % {:directive :foo
                                  :category category
                                  :arguments (:directive-args execution-context)}))
  (resolver execution-context field-selection))

(defmulti bar-visitor :category)

(defmethod bar-visitor :default [{:keys [category resolver execution-context field-selection]}]
  (swap! visitor-record #(conj % {:directive :bar
                                  :category category
                                  :arguments (:directive-args execution-context)}))
  (is (:previous-directive-value execution-context))
  (resolver execution-context field-selection))

(defmulti baz-visitor :category)

(defmethod baz-visitor :default [{:keys [category resolver execution-context field-selection]}]
  (swap! visitor-record #(conj % {:directive :baz
                                  :category category
                                  :arguments (:directive-args execution-context)}))
  (is (:previous-directive-value execution-context))
  (resolver execution-context field-selection))

(def locations
  #{:object
    :field-definition
    :enum
    :enum-value
    :scalar
    :argument-definition
    :schema
    :interface
    :union
    :input-object
    :input-field-definition})

(def default-schema
  (-> test-schema
      (assoc :directive-defs {:foo {:directive-args {:foo-argument 'String}
                                    :locations locations}
                              :bar {:directive-args {:bar-argument 'String}
                                    :locations locations}
                              :baz {:directive-args {:baz-argument 'String}
                                    :locations locations}})))

(def foo-directive-spec
  {:directive-type :foo
   :directive-args {:foo-argument "foo-value"}})

(def bar-directive-spec
  {:directive-type :bar
   :directive-args {:bar-argument "bar-value"}})

(def baz-directive-spec
  {:directive-type :baz
   :directive-args {:baz-argument "baz-value"}})

(defn assert-visits [directives {:keys [type directive-path query expected-result update-fn]}]
  (reset! visitor-record [])
  (let [directive-visitors {:foo foo-visitor
                            :bar bar-visitor
                            :baz baz-visitor}
        update-schema-with-fn (fn update-schema-with-fn [schema directive-path update-fn]
                                (if (seq directive-path)
                                  (update-in schema directive-path update-fn)
                                  (update-fn schema)))

        schema (cond-> default-schema
                   (nil? update-fn) (assoc-in directive-path directives)
                   update-fn (update-schema-with-fn directive-path update-fn))

        compiled-schema (schema/compile schema {:default-field-resolver schema/hyphenating-default-field-resolver
                                                :directive-visitors directive-visitors})
        actual-query-result (execute compiled-schema query nil nil)]

    (testing (str "Visits " type " directives")
      (is (seq @visitor-record))
      (is (= (count directives) (count @visitor-record)))
      (is (= (repeat (count directives) type)
             (mapv :category @visitor-record)))
      (testing "Produces expected result"
        (is (= expected-result actual-query-result)))
      (testing "Visits in correct order"
        (is (= (mapv :directive-type directives)
               (mapv :directive @visitor-record))))
      (testing "Receives directive arguments"
        (is (= (mapv :directive-args directives)
               (mapv :arguments @visitor-record)))))))

(deftest visitors
  (doseq [directives [[foo-directive-spec]
                      [foo-directive-spec bar-directive-spec]
                      [foo-directive-spec bar-directive-spec baz-directive-spec]]]
    (are [test-case] (assert-visits directives test-case)
      {:type :object
       :directive-path [:objects :human :directives]
       :query "query FetchLukeQuery { human(id: \"1000\") { name }}"
       :expected-result {:data {:human {:name "Luke Skywalker"}}}}

      {:type :field
       :directive-path [:objects :human :fields :name :directives]
       :query "query FetchLukeQuery { human(id: \"1000\") { name }}"
       :expected-result {:data {:human {:name "Luke Skywalker"}}}}

      {:type :enum
       :directive-path [:enums :episode :directives]
       :query "{ hero { id name appears_in } }"
       :expected-result {:data {:hero {:id "2001"
                                       :name "R2-D2"
                                       :appears_in [:NEWHOPE :EMPIRE :JEDI]}}}}

      {:type :enum-value
       :directive-path [:enums :episode :values]
       :update-fn #(map (fn [v] {:enum-value v :directives directives})
                        %)
       :query "{ human(id: \"1004\") { id name appears_in } }"
       :expected-result {:data {:human {:id "1004"
                                        :name "Wilhuff Tarkin"
                                        :appears_in [:NEWHOPE]}}}}

      {:type :scalar
       :directive-path [:scalars :Date :directives]
       :query "{ now {date} }"
       :expected-result {:data {:now {:date "A long time ago"}}}}

      {:type :argument-definition
       :directive-path [:queries :human :args :id :directives]
       :query "{ human(id: \"1000\") { name } }"
       :expected-result {:data {:human {:name "Luke Skywalker"}}}}

      {:type :schema
       :directive-path [:directives]
       :query "{ human(id: \"1000\") { name } }"
       :expected-result {:data {:human {:name "Luke Skywalker"}}}}

      {:type :interface
       :directive-path [:interfaces :character :directives]
       :query "query UseFragment {
             heroes: hero(episode: NEWHOPE) {
               id
               forceSide { name }
               ... on human {
                 name
                 homePlanet
               }
             }
           }"
       :expected-result {:data
                         {:heroes
                          {:id "1000",
                           :forceSide {:name "light"},
                           :name "Luke Skywalker",
                           :homePlanet "Tatooine"}}}}

      {:type :input-object
       :directive-path [:input-objects :testInputObject :directives]
       :query "query UseInputObject {
             thing: echoArgs(integer: 12, integerArray: [1, 2], inputObject: {integer: 3, string: \"Text\", nestedInputObject: {}}) {
               integer
               integerArray
               inputObject
             }
           }"
       :expected-result {:data
                         {:thing
                          {:integer 12,
                           :integerArray '(1 2),
                           :inputObject
                           "{:integer 3, :string \"Text\", :nestedInputObject {}}"}}}}

      {:type :input-field-definition
       :directive-path [:input-objects :testInputObject :fields :integer :directives]
       :query "query UseInputObject {
             thing: echoArgs(integer: 12, integerArray: [1, 2], inputObject: {integer: 3, string: \"Text\", nestedInputObject: {}}) {
               integer
               integerArray
               inputObject
             }
           }"
       :expected-result {:data
                         {:thing
                          {:integer 12,
                           :integerArray '(1 2),
                           :inputObject
                           "{:integer 3, :string \"Text\", :nestedInputObject {}}"}}}}
      {:type :union
       :directive-path []
       :update-fn (fn [schema]
                    (-> schema
                        (update :interfaces (fn [i] (assoc i :ship {:fields {:name {:type 'String}}})))
                        (assoc :unions {:human_or_droid {:members [:human :droid]
                                                         :directives directives}})
                        (update :queries (fn [q] (assoc q :thing {:type '(non-null :human_or_droid)
                                                                  :args {:episode {:type :episode}}
                                                                  :resolve (fn [ctx args v]
                                                                             (let [{:keys [episode]} args]
                                                                               (ts/get-hero episode)))})))))
       :query "query UseUnion {
             luke: thing(episode: NEWHOPE) {
               ... on droid { accessories }
               ... on human { name }
             }
           }"
       :expected-result {:data {:luke {:name "Luke Skywalker"}}}})))
