(ns com.walmartlabs.lacinia.directives
  (:require [com.walmartlabs.lacinia.constants :as constants]))

(defn directive->visitor [schema]
  (fn [{:keys [directive-type]}]
    (get-in schema [constants/directive-visitors-key directive-type])))

(defn build-visitor
  ([schema field-type resolver directives-for-type]
   (let [allowed-directives (-> schema constants/directive-definitions-key keys set)
         applicable-directives (->> directives-for-type
                                    (filter #(allowed-directives (:directive-type %))))]
     (or (first (map (directive->visitor schema)
                     applicable-directives))
         (fn [{:keys [execution-context field-selection]}]
           (resolver execution-context field-selection)))))
  ([schema field-type resolver]
   (build-visitor schema field-type resolver (get-in schema
                                                     (if field-type
                                                       [field-type :directives]
                                                       [:com.walmartlabs.lacinia.schema/directives])))))
