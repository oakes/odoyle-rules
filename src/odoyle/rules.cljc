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
                      children ;; vector of AlphaNode
                      successors ;; vector of JoinNode
                      ])
(defrecord MemoryNode [path ;; the get-in vector to reach this node from the root
                       child ;; JoinNode
                       rule-name ;; keyword
                       ])
(defrecord JoinNode [path ;; the get-in vector to reach this node from the root
                     child ;; MemoryNode
                     alpha-node-path ;; the get-in vector to reach the parent AlphaNode from the root
                     condition ;; Condition
                     ])
(defrecord Condition [nodes ;; vector of AlphaNode
                      vars ;; vector of Var
                      rule-name ;; keyword
                      ])
(defrecord Rule [name ;; keyword
                 conditions ;; vector of Condition
                 rule-fn ;; fn
                 ])
(defrecord Session [root-node ;; AlphaNode
                    rule-fns ;; fns
                    ])

(defn- add-to-condition [condition field [kind value]]
  (case kind
    :binding (update condition :vars conj (->Var field value))
    :value (update condition :nodes conj (->AlphaNode field value [] []))))

(defn- ->condition [{:keys [id attr value]}]
  (-> {:vars [] :nodes []}
      (add-to-condition :id id)
      (add-to-condition :attr attr)
      (add-to-condition :value value)))

(defn- ->rule [[rule-name rule]]
  (let [{:keys [what-block when-block then-block]} rule
        conditions (mapv ->condition (:body what-block))]
    [rule-name conditions (:body then-block)]))

(defn- add-alpha-node [node new-nodes *alpha-node-path]
  (let [[new-node & other-nodes] new-nodes]
    (if new-node
      (if-let [i (->> (:children node)
                      (map-indexed vector)
                      (some (fn [[i child]]
                              (when (= (select-keys child [:test-field :test-value])
                                       (select-keys new-node [:test-field :test-value]))
                                i))))]
        (do
          (vswap! *alpha-node-path conj :children i)
          (update node :children update i add-alpha-node other-nodes *alpha-node-path))
        (do
          (vswap! *alpha-node-path conj :children (-> node :children count))
          (update node :children conj (add-alpha-node new-node other-nodes *alpha-node-path))))
      node)))

(defn- add-condition [session condition]
  (let [*alpha-node-path (volatile! [])
        session (update session :root-node add-alpha-node (:nodes condition) *alpha-node-path)
        alpha-node-path @*alpha-node-path
        successor-count (count (get-in (:root-node session) (conj alpha-node-path :successors)))
        join-node-path (conj alpha-node-path :successors successor-count)
        mem-node-path (conj join-node-path :child)
        mem-node (->MemoryNode mem-node-path nil (:rule-name condition))
        join-node (->JoinNode join-node-path mem-node alpha-node-path condition)]
    (update session :root-node update-in alpha-node-path update :successors conj join-node)))

;; public

(defn add-rule [session rule]
  (let [conditions (:conditions rule)
        conditions (assoc-in conditions [(dec (count conditions)) :rule-name] (:name rule))
        rule-path [:rule-fns (:name rule)]]
    (when (get-in session rule-path)
      (throw (ex-info (str (:name rule) " already exists in session") {})))
    (-> (reduce add-condition session conditions)
        (assoc-in rule-path (:rule-fn rule)))))

(defmacro ruleset [rules]
  (reduce
    (fn [v [rule-name conditions fn-body]]
      (conj v `(->Rule ~rule-name
                       (mapv map->Condition ~conditions)
                       (fn [] ~@fn-body))))
    []
    (mapv ->rule (parse ::rules rules))))

(defn ->session []
  (->Session (->AlphaNode nil nil [] []) {}))

(defn retract! [fact])

