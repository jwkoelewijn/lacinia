(ns com.walmartlabs.lacinia.directives
  (:require [com.walmartlabs.lacinia.constants :as constants]))

(defn directive->visitor [schema]
  (fn [{:keys [directive-type] :as directive}]
    [directive (get-in schema [constants/directive-visitors-key directive-type])]))

(defn has-visitor? [schema directive-type]
  (let [visitors (get schema constants/directive-visitors-key)]
    (contains? visitors directive-type)))

(defn build-visitor
  ([schema directives-for-type]
   (let [allowed-directives (-> schema constants/directive-definitions-key keys set)
         applicable-directives (->> directives-for-type
                                    (filter #(allowed-directives (:directive-type %)))
                                    (filter #(has-visitor? schema (:directive-type %))))
         chain (map (directive->visitor schema)
                    applicable-directives)]
     (fn [{:keys [category execution-context field-selection resolver]}]
       (if (not (seq chain))
         (resolver execution-context field-selection)
         (loop [[[directive visitor] & rest] chain
                ctx execution-context]
           (if visitor
             (recur rest (assoc ctx :previous-directive-value (visitor {:execution-context (assoc ctx :directive-args (:directive-args directive)
                                                                                                      :current-directive directive)
                                                                        :field-selection field-selection
                                                                        :resolver resolver
                                                                        :category category})))
             (:previous-directive-value ctx))))))))

(defn build-visitor-for-type [schema type]
  (build-visitor schema (get-in schema
                                (if type
                                  [type :directives]
                                  [:com.walmartlabs.lacinia.schema/directives]))))