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
(defrecord AlphaNode [id ;; int
                      test-field ;; :id, :attr, or :value
                      test-value ;; anything
                      children ;; vector of AlphaNode
                      ])
(defrecord Condition [nodes ;; vector of AlphaNode
                      vars ;; vector of Var
                      ])
(defrecord Rule [conditions ;; vector of Condition
                 callback ;; fn
                 ])
(defrecord Session [alpha-node])

(defn- add-to-condition [condition field [kind value]]
  (case kind
    :binding (update condition :vars conj (->Var field value))
    :value (update condition :nodes conj (->AlphaNode nil field value []))))

(defn- ->condition [{:keys [id attr value]}]
  (-> (->Condition [] [])
      (add-to-condition :id id)
      (add-to-condition :attr attr)
      (add-to-condition :value value)))

(defn- ->rule [rule-name rule]
  (let [{:keys [what-block when-block then-block]} rule
        conditions (mapv ->condition (:body what-block))
        callback `(fn [] ~@(:body then-block))]
    (->Rule conditions callback)))

(def ^:private ^:dynamic *last-id* nil)
(def ^:private ^:dynamic *leaf-id* nil)

(defn- add-node [node new-nodes]
  (reset! *leaf-id* (:id node))
  (let [[new-node & other-nodes] new-nodes]
    (if new-node
      (if-let [i (->> (:children node)
                      (map-indexed vector)
                      (some (fn [[i child]]
                              (when (= (select-keys child [:test-field :test-value])
                                       (select-keys new-node [:test-field :test-value]))
                                i))))]
        (update node :children update i add-node other-nodes)
        (let [new-node (assoc new-node :id (swap! *last-id* inc))]
          (update node :children conj (add-node new-node other-nodes))))
      node)))

(defn- add-nodes [session nodes]
  (binding [*leaf-id* (atom -1)]
    [(update session :alpha-node add-node nodes)
     @*leaf-id*]))

(defn- add-rule [session rule]
  (reduce
    (fn [session condition]
      (let [[session leaf-id] (add-nodes session (:nodes condition))]
        session))
    session
    (:conditions rule)))

;; public

(defmacro ruleset [rules]
  (->> rules
       (parse ::rules)
       (reduce
         (fn [m [rule-name rule]]
           (assoc m rule-name (->rule rule-name rule)))
         {})))

(defn ->session [rules]
  (binding [*last-id* (atom -1)]
    (reduce
      add-rule
      (->Session (->AlphaNode (swap! *last-id* inc) nil nil []))
      (vals rules))))

