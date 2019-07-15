(ns com.walmartlabs.lacinia.directives)

(defn directive-resolver-chain-for [schema directives]
  (if-not (seq directives)
    ;; no need to do anything
    (constantly true)
    (let [allowed-directives (->> (:com.walmartlabs.lacinia.schema/directive-defs schema)
                                  keys
                                  set)
          applicable-directives (->> directives
                                     (filter (fn [{:keys [directive-type]}]
                                               (allowed-directives directive-type))))
          directive-resolvers (->> applicable-directives
                                   (map (fn [{:keys [directive-type] :as directive}]
                                          (assoc directive :resolver (get-in schema [:com.walmartlabs.lacinia.schema/directive-resolvers directive-type])))))]
      (fn [context element]
        (reduce (fn [res {:keys [directive-args resolver]}]
                  (and res (resolver context directive-args element)))
                true
                directive-resolvers)))))
