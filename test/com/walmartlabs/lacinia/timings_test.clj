; Copyright (c) 2017-present Walmart, Inc.
;
; Licensed under the Apache License, Version 2.0 (the "License")
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

(ns com.walmartlabs.lacinia.timings-test
  "Tests for the optional timing logic."
  (:require
    [clojure.test :refer [deftest is]]
    [clojure.java.io :as io]
    [clojure.edn :as edn]
    [com.walmartlabs.test-reporting :refer [reporting]]
    [com.walmartlabs.lacinia.util :as util]
    [com.walmartlabs.lacinia.resolve :as resolve]
    [com.walmartlabs.test-utils :refer [simplify]]
    [com.walmartlabs.lacinia :refer [execute]]
    [com.walmartlabs.lacinia.schema :as schema]))

(def ^:private enable-timing {:com.walmartlabs.lacinia/enable-timing? true})

(defn ^:private resolve-fast
  [_ args _]
  {:simple (:value args)
   ::slow {:simple (:nested_value args)}
   ::delay (:delay args)})

(defn ^:private resolve-slow
  [_ _ value]
  (let [resolved-value (resolve/resolve-promise)
        f (fn []
            (Thread/sleep (::delay value))
            (resolve/deliver! resolved-value (::slow value)))
        thread (Thread. ^Runnable f)]
    (.start thread)
    resolved-value))

(def ^:private compiled-schema
  (-> (io/resource "timing-schema.edn")
      slurp
      edn/read-string
      (util/attach-resolvers {:resolve-fast resolve-fast
                              :resolve-slow resolve-slow})
      schema/compile))

(defn ^:private q
  ([query]
   (q query nil))
  ([query context]
   (-> (execute compiled-schema query nil context)
       simplify)))

(defn ^:private timing-for
  [result path]
  (->> result
       :extensions
       :timings
       (filter #(= path (:path %)))
       first))

(deftest timings-are-off-by-default
  (is (= {:data {:root {:simple "fast!"
                        :slow {:simple "slow!!"}}}}
         (q "{ root(delay: 50) { simple slow { simple }}}"))))

(deftest timing-is-collected-when-enabled
  (let [result (q "{ root(delay: 50) { simple slow { simple }}}" enable-timing)]
    (is (-> result :extensions :timings seq)
        "Some timings were collected.")))

(deftest does-not-collect-timing-for-default-resolvers
  (let [result (q "{ root(delay: 50) { simple slow { simple }}}" enable-timing)]
    (reporting result
      (is (= nil (timing-for result [:root]))))))

(deftest collects-timing-for-provided-resolvers
  (doseq [delay [25 50 75]
          :let [result (q (str "{ root(delay: " delay ") { slow { simple }}}") enable-timing)
                slow-timing (timing-for result [:root :slow])
                {:keys [start finish elapsed]} slow-timing]]
    (reporting result
      ;; Allow for a bit of overhead; Thread/sleep is quite inexact.
      (is (<= delay elapsed (* delay 10)))
      ;; Check that :start and :finish are both present and add up
      (is (= elapsed (- (Long/parseLong finish) (Long/parseLong start)))))))

(deftest collects-timing-for-each-execution
  (let [result (q "{ hare: root(delay: 5) { slow { simple }}
                     tortoise: root(delay: 50) { slow { simple }}
                   }"
                  enable-timing)]
    (reporting result
      (let [elapsed-times (->> (get-in result [:extensions :timings])
                               (mapv :elapsed))]
        (is (= 2 (count elapsed-times)))
        (is (<= 5 (elapsed-times 0)))
        (is (<= 50 (elapsed-times 1)))))))

