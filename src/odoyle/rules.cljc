(ns odoyle.rules
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]))

;; parsing

(s/def ::what-id (s/or :binding simple-symbol? :value qualified-keyword?))
(s/def ::what-attr (s/or :value qualified-keyword?))
(s/def ::what-value (s/or :binding simple-symbol? :value any?))
(s/def ::what-tuple (s/cat :id ::what-id, :attr ::what-attr, :value ::what-value))
(s/def ::what-block (s/cat :header #{:what} :body (s/+ (s/spec ::what-tuple))))
(s/def ::when-block (s/cat :header #{:when} :body (s/+ #(not (keyword? %)))))
(s/def ::then-block (s/cat :header #{:then} :body (s/+ #(not (keyword? %)))))

(s/def ::rule (s/cat
                :what-block ::what-block
                :when-block (s/? ::when-block)
                :then-block (s/? ::then-block)))

(s/def ::rules (s/map-of qualified-keyword? ::rule))

(defn- parse [spec content]
  (let [res (s/conform spec content)]
    (if (= ::s/invalid res)
      (throw (ex-info (expound/expound-str spec content) {}))
      res)))

;; private

(defrecord Var [field ;; :id, :attr, or :value
                sym ;; symbol
                ])
(defrecord AlphaNode [test-field ;; :id, :attr, or :value
                      test-value ;; anything
                      ])
(defrecord Condition [nodes ;; vector of AlphaNode
                      vars ;; vector of Var
                      ])
(defrecord Production [conditions ;; vector of Condition
                       ])
(defrecord Session [])

(defn- add-to-condition [condition field [kind value]]
  (case kind
    :binding (update condition :vars conj (->Var field value))
    :value (update condition :nodes conj (->AlphaNode field value))))

(defn- ->condition [{:keys [id attr value]}]
  (-> (->Condition [] [])
      (add-to-condition :id id)
      (add-to-condition :attr attr)
      (add-to-condition :value value)))

(defn- ->production [[rule-name rule]]
  (let [{:keys [what-block when-block then-block]} rule
        conditions (mapv ->condition (:body what-block))]
    (->Production conditions)))

;; public

(defmacro ruleset [rules]
  (->> rules
       (parse ::rules)
       (mapv ->production)))

(defn ->session [rules])

