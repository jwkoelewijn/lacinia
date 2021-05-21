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

(ns ^:no-doc com.walmartlabs.lacinia.constants
  "A handy place to define namespaced constants")

(def schema-key
  "Context key storing the compiled schema."
  ::schema)

(def parsed-query-key
  "Context key storing the parsed and prepared query."
  ::parsed-query)

(def ^{:added "0.17.0"} selection-key
  "Context key storing the current selection."
  :com.walmartlabs.lacinia/selection)

(def ^{:added "0.34.0"} directive-definitions-key
  "Context key storing directive visitors."
  :com.walmartlabs.lacinia.schema/directive-defs)

(def ^{:added "0.34.0"} directive-visitors-key
  "Context key storing directive visitors."
  :com.walmartlabs.lacinia.schema/directive-visitors)
