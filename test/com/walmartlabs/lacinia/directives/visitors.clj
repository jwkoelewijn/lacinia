(ns com.walmartlabs.lacinia.directives.visitors
  (:require [clojure.test :refer :all]
            [com.walmartlabs.lacinia.util :as util]
            [com.walmartlabs.lacinia.schema :as schema]
            [com.walmartlabs.test-schema :refer [test-schema] :as ts]
            [com.walmartlabs.lacinia :refer [execute]]))


(def visitor-record (atom {}))

(use-fixtures :each (fn [t]
                      (reset! visitor-record {})
                      (t)))

(defmulti tracer-directive-visitor (fn [{:keys [category]}] category))

(defmethod tracer-directive-visitor :default [{:keys [category resolver execution-context field-selection]}]
  (println "Visit" category)
  (swap! visitor-record #(assoc-in % [:tracer category] true))
  (resolver execution-context field-selection))

(def default-schema
  (-> test-schema
      (assoc :directive-defs {:tracer
                              {:locations
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
                                 :input-field-definition}}})))


(defn assert-visits [{:keys [type directive-path query expected-result update-fn]}]
  (let [directive-visitors {:tracer tracer-directive-visitor}
        schema (cond-> default-schema
                   (nil? update-fn) (assoc-in directive-path [{:directive-type :tracer}])
                   update-fn (update-in directive-path update-fn))
        compiled-schema (schema/compile schema {:default-field-resolver schema/hyphenating-default-field-resolver
                                                :directive-visitors directive-visitors})
        actual-query-result (execute compiled-schema query nil nil)]
    (testing (str "Visits " type " directives")
      (is (= expected-result actual-query-result))
      (is (get-in @visitor-record [:tracer type])))))

(deftest visitors
  (are [test-case] (assert-visits test-case)
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
     :update-fn #(map (fn [v] {:enum-value v :directives [{:directive-type :tracer}]})
                      %)
     :query "{ hero { id name appears_in } }"
     :expected-result {:data {:hero {:id "2001"
                                     :name "R2-D2"
                                     :appears_in [:NEWHOPE :EMPIRE :JEDI]}}}}

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
                         "{:integer 3, :string \"Text\", :nestedInputObject {}}"}}}}))

(deftest visit-union
  (let [directive-visitors {:tracer tracer-directive-visitor}
        schema (-> default-schema
                   (update :interfaces (fn [i] (assoc i :ship {:fields {:name {:type 'String}}})))
                   (assoc :unions {:human_or_droid {:members [:human :droid]
                                                    :directives [{:directive-type :tracer}]}})
                   (update :queries (fn [q] (assoc q :thing {:type '(non-null :human_or_droid)
                                                             :args {:episode {:type :episode}}
                                                             :resolve (fn [ctx args v]
                                                                        (let [{:keys [episode]} args]
                                                                          (ts/get-hero episode)))}))))
        compiled-schema (schema/compile schema {:directive-visitors directive-visitors})

        q "query UseUnion {
             luke: thing(episode: NEWHOPE) {
               ... on droid { accessories }
               ... on human { name }
             }
           }"
        res (execute compiled-schema q {} nil)]
    (is (= {:data {:luke {:name "Luke Skywalker"}}}
           res))
    (is (get-in @visitor-record [:tracer :union]))))
