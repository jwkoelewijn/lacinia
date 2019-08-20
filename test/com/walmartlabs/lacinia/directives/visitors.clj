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

(defmethod tracer-directive-visitor :default [{:keys [category resolver execution-context field-selection] :as visitor-context}]
  (println "Visit" category)
  (swap! visitor-record #(assoc-in % [:tracer category] true))
  (resolver execution-context field-selection))

(def default-schema
  (-> test-schema
      (assoc :directive-defs {:tracer {:locations #{:object :field-definition :enum :enum-value :scalar :argument-definition :schema :interface :union :input-object :input-field-definition}
                                       :args {:verbose {:type :Boolean}}}})))

(deftest visits-objects
  (let [directive-visitors {:tracer tracer-directive-visitor}
        schema (-> default-schema
                   (assoc-in [:objects :human :directives] [{:directive-type :tracer
                                                             :directive-args {:verbose true}}]))
        compiled-schema (schema/compile schema {:directive-visitors directive-visitors})
        q "query FetchLukeQuery {
             human(id: \"1000\") {
               name
             }
           }"]
    (execute compiled-schema q nil nil)
    (testing "Visits :object directive visitor"
      (is (get-in @visitor-record [:tracer :object])))))

(deftest visits-field-definitions
  (let [directive-visitors {:tracer tracer-directive-visitor}
        schema (-> default-schema
                   (assoc-in [:objects :human :fields :name :directives] [{:directive-type :tracer
                                                                           :directive-args {:verbose true}}]))
        compiled-schema (schema/compile schema {:directive-visitors directive-visitors})

        q "query FetchLukeQuery {
             human(id: \"1000\") {
               name
             }
           }"
        result (execute compiled-schema q nil nil)]
    (testing "Visits :object directive visitor"
      (is (= {:data {:human {:name "Luke Skywalker"}}}
             result))
      (is (get-in @visitor-record [:tracer :field])))))

(deftest visits-enums
  (let [directive-visitors {:tracer tracer-directive-visitor}
        schema (-> default-schema
                   (assoc-in [:enums :episode :directives] [{:directive-type :tracer
                                                             :directive-args {:verbose true}}]))

        compiled-schema (schema/compile schema {:default-field-resolver schema/hyphenating-default-field-resolver
                                                :directive-visitors directive-visitors})

        q "{ hero { id name appears_in } }"]
    (testing "Visits :enum directive visitor"
      (is (= {:data {:hero {:id "2001"
                            :name "R2-D2"
                            :appears_in [:NEWHOPE :EMPIRE :JEDI]}}}
             (execute compiled-schema q {} nil)))
      (is (get-in @visitor-record [:tracer :enum])))))

(deftest visits-enum-values
  (let [directive-visitors {:tracer tracer-directive-visitor}
        schema (-> default-schema
                   (update-in [:enums :episode :values] #(map (fn [v] {:enum-value v :directives [{:directive-type :tracer
                                                                                                   :directive-args {:verbose true}}]})
                                                              %)))

        compiled-schema (-> (schema/compile schema {:default-field-resolver schema/hyphenating-default-field-resolver
                                                    :directive-visitors directive-visitors}))
        q "{ hero { id name appears_in } }"]
    (testing "Visits :enum directive visitor"
      (is (= {:data {:hero {:id "2001"
                            :name "R2-D2"
                            :appears_in [:NEWHOPE :EMPIRE :JEDI]}}}
             (execute compiled-schema q {} nil)))
      (is (get-in @visitor-record [:tracer :enum-value])))))

(deftest visit-scalars
  (let [directive-visitors {:tracer tracer-directive-visitor}
        schema (-> default-schema
                   (assoc-in [:scalars :Date :directives] [{:directive-type :tracer
                                                            :directive-args {:verbose true}}]))
        compiled-schema (schema/compile schema {:directive-visitors directive-visitors})

        q "{ now {date} }"]
    (testing "Visits :scalar directives"
      (is (= {:data {:now {:date "A long time ago"}}}
             (execute compiled-schema q {} nil)))
      (is (get-in @visitor-record [:tracer :scalar])))))

(deftest visit-argument-definitions
  (let [directive-visitors {:tracer tracer-directive-visitor}
        schema (-> default-schema
                   (assoc-in [:objects :human :directives] [{:directive-type :tracer
                                                             :directive-args {:verbose true}}])
                   (assoc-in [:queries :human :args :id :directives] [{:directive-type :tracer
                                                                       :directive-args {:verbose true}}]))
        compiled-schema (schema/compile schema {:directive-visitors directive-visitors})

        q "{ human(id: \"1000\") { name } }"]
    (testing "Visits :argument-definition directives"
      (is (= {:data {:human {:name "Luke Skywalker"}}}
             (execute compiled-schema q {} nil)))
      (is (get-in @visitor-record [:tracer :argument-definition])))))

(deftest visit-schema
  (let [directive-visitors {:tracer tracer-directive-visitor}
        schema (-> default-schema
                   (assoc :directives [{:directive-type :tracer
                                        :directive-args {:verbose true}}]))
        compiled-schema (schema/compile schema {:directive-visitors directive-visitors})

        q "{ human(id: \"1000\") { name } }"]
    (testing "Visits :argument-definition directives"
      (is (= {:data {:human {:name "Luke Skywalker"}}}
             (execute compiled-schema q {} nil)))
      (is (get-in @visitor-record [:tracer :schema])))))

(deftest visit-interface
  (let [directive-visitors {:tracer tracer-directive-visitor}
        schema (-> default-schema
                   (assoc-in [:interfaces :character :directives] [{:directive-type :tracer
                                                                    :directive-args {:verbose true}}]))
        compiled-schema (schema/compile schema {:directive-visitors directive-visitors})

        q "query UseFragment {
             heroes: hero(episode: NEWHOPE) {
               id
               forceSide { name }
               ... on human {
                 name
                 homePlanet
               }
             }
           }"
        res (execute compiled-schema q {} nil)]
    (is (= {:data
            {:heroes
             {:id "1000",
              :forceSide {:name "light"},
              :name "Luke Skywalker",
              :homePlanet "Tatooine"}}}
           res))
    (is (get-in @visitor-record [:tracer :interface]))))

(deftest visit-union
  (let [directive-visitors {:tracer tracer-directive-visitor}
        schema (-> default-schema
                   (update :interfaces (fn [i] (assoc i :ship {:fields {:name {:type 'String}}})))
                   (assoc :unions {:human_or_droid {:members [:human :droid]
                                                    :directives [{:directive-type :tracer
                                                                  :directive-args {:verbose true}}]}})
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

(deftest visit-input-object
  (let [directive-visitors {:tracer tracer-directive-visitor}
        schema (-> default-schema
                   (assoc-in [:input-objects :testInputObject :directives] [{:directive-type :tracer
                                                                             :directive-args {:verbose true}}]))
        compiled-schema (schema/compile schema {:directive-visitors directive-visitors})

        q "query UseInputObject {
             thing: echoArgs(integer: 12, integerArray: [1, 2], inputObject: {integer: 3, string: \"Text\", nestedInputObject: {}}) {
               integer
               integerArray
               inputObject
             }
           }"
        res (execute compiled-schema q {} nil)]
   (is (= {:data
           {:thing
            {:integer 12,
             :integerArray '(1 2),
             :inputObject
             "{:integer 3, :string \"Text\", :nestedInputObject {}}"}}}
          res))
   (is (get-in @visitor-record [:tracer :input-object]))))

(deftest visit-input-field-definition
  (let [directive-visitors {:tracer tracer-directive-visitor}
        schema (-> default-schema
                   (assoc-in [:input-objects :testInputObject :fields :integer :directives] [{:directive-type :tracer
                                                                                              :directive-args {:verbose true}}]))
        compiled-schema (schema/compile schema {:directive-visitors directive-visitors})

        q "query UseInputObject {
             thing: echoArgs(integer: 12, integerArray: [1, 2], inputObject: {integer: 3, string: \"Text\", nestedInputObject: {}}) {
               integer
               integerArray
               inputObject
             }
           }"
        res (execute compiled-schema q {} nil)]
    (is (= {:data
            {:thing
             {:integer 12,
              :integerArray '(1 2),
              :inputObject
              "{:integer 3, :string \"Text\", :nestedInputObject {}}"}}}
           res))
    (is (get-in @visitor-record [:tracer :input-field-definition]))))

(comment
 (run-tests))
