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

(defrecord Fact [id attr value])
(defrecord Token [fact ;; Fact
                  kind ;; :insert, :retract, :update
                  old-fact ;; only used when updating
                  ])
(defrecord Var [field ;; :id, :attr, or :value
                sym ;; symbol
                ])
(defrecord AlphaNode [path ;; the get-in vector to reach this node from the root
                      test-field ;; :id, :attr, or :value
                      test-value ;; anything
                      children ;; vector of AlphaNode
                      successors ;; vector of JoinNode
                      facts ;; map of id -> (map of attr -> Fact)
                      ])
(defrecord MemoryNode [path ;; the get-in vector to reach this node from the root
                       child-path ;; the get-in vector to reach the child JoinNode
                       rule-name ;; keyword
                       vars ;; vector of (map of sym -> value)
                       id-attrs ;; vector of id+attr
                       ])
(defrecord JoinNode [path ;; the get-in vector to reach this node from the root
                     parent-path ;; the get-in vector to reach the parent MemoryNode
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
                    id-attr-nodes ;; map of id+attr -> set of alpha node paths
                    ])

(defn- add-to-condition [condition field [kind value]]
  (case kind
    :binding (update condition :vars conj (->Var field value))
    :value (update condition :nodes conj (map->AlphaNode {:path nil
                                                          :test-field field
                                                          :test-value value
                                                          :children []
                                                          :successors []
                                                          :facts {}}))))

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
        (let [path (vswap! *alpha-node-path conj :children (-> node :children count))
              new-node (assoc new-node :path path)]
          (update node :children conj (add-alpha-node new-node other-nodes *alpha-node-path))))
      node)))

(defn- add-condition [session condition]
  (let [*alpha-node-path (volatile! [:root-node])
        session (update session :root-node add-alpha-node (:nodes condition) *alpha-node-path)
        alpha-node-path @*alpha-node-path
        successor-count (count (get-in session (conj alpha-node-path :successors)))
        join-node-path (conj alpha-node-path :successors successor-count)
        mem-node-path (conj join-node-path :child)
        parent-mem-node-path (:mem-node-path session)
        mem-node (map->MemoryNode {:path mem-node-path
                                   :child-path nil
                                   :rule-name (:rule-name condition)
                                   :vars []
                                   :id-attrs []})
        join-node (map->JoinNode {:path join-node-path
                                  :parent-path parent-mem-node-path
                                  :child mem-node
                                  :alpha-node-path alpha-node-path
                                  :condition condition})]
    (-> session
        (assoc-in join-node-path join-node)
        (cond-> parent-mem-node-path
                (update-in parent-mem-node-path assoc :child-path join-node-path))
        (assoc :mem-node-path mem-node-path))))

(defn- get-vars-from-fact [vars condition fact]
  (reduce
    (fn [m cond-var]
      (let [sym (:sym cond-var)]
        (case (:field cond-var)
          :id
          (if (and (contains? m sym)
                   (not= (get m sym) (:id fact)))
            (reduced nil)
            (assoc m sym (:id fact)))
          :attr
          (throw (ex-info "Attributes cannot contain vars" {}))
          :value
          (if (and (contains? m sym)
                   (not= (get m sym) (:value fact)))
            (reduced nil)
            (assoc m sym (:value fact))))))
    vars
    (:vars condition)))

(defn- perform-join-tests [node vars alpha-fact]
  (not-empty (get-vars-from-fact vars (:condition node) alpha-fact)))

(defn- dissoc-vec [v index]
  (let [v1 (subvec v 0 index)
        v2 (subvec v (inc index))]
    (into (into [] v1) v2)))

(defn- left-activate-join-node [session node vars token]
  session)

(defn- left-activate-memory-node [session node-path vars token]
  (let [{:keys [id attr] :as fact} (:fact token)
        id+attr [id attr]
        node (get-in session node-path)]
    (as-> session $
          (case (:kind token)
            :insert
            (update-in session node-path
                       (fn [node]
                         (-> node
                             (update :vars conj vars)
                             (update :id-attrs conj id+attr))))
            :retract
            (let [index (.indexOf (:id-attrs node) id+attr)]
              (assert (>= index 0))
              (update-in session node-path
                         (fn [node]
                           (-> node
                               (update :vars dissoc-vec index)
                               (update :id-attrs dissoc-vec index)))))
            :update
            (let [index (.indexOf (:id-attrs node) id+attr)]
              (assert (>= index 0))
              (update-in session node-path assoc-in [:vars index] vars)))
          (if-let [join-node-path (:child-path node)]
            (left-activate-join-node $ join-node-path vars token)
            $))))

(defn- right-activate-join-node [session node-path token]
  (let [node (get-in session node-path)]
    (if-let [parent-path (:parent-path node)]
      (reduce
        (fn [session existing-vars]
          (if-let [vars (perform-join-tests node existing-vars (:fact token))]
            (left-activate-memory-node session (-> node :child :path) vars token)
            session))
        session
        (:vars (get-in session parent-path)))
      ;; root node
      (if-let [vars (perform-join-tests node {} (:fact token))]
        (left-activate-memory-node session (-> node :child :path) vars token)
        session))))

(defn- right-activate-alpha-node [session node-path token]
  (let [{:keys [id attr] :as fact} (:fact token)
        id+attr [id attr]]
    (as-> session $
          (case (:kind token)
            :insert
            (-> $
                (update-in node-path assoc-in [:facts id attr] fact)
                (update-in [:id-attr-nodes id+attr]
                           (fn [node-paths]
                             (let [node-paths (or node-paths #{})]
                               (assert (not (contains? node-paths node-path)))
                               (conj node-paths node-path)))))
            :retract
            (-> $
                (update-in node-path update-in [:facts id] dissoc attr)
                (update-in [:id-attr-nodes id+attr]
                           (fn [node-paths]
                             (assert (contains? node-paths node-path))
                             (disj node-paths node-path))))
            :update
            (update-in $ node-path update-in [:facts id attr]
                       (fn [old-fact]
                         (assert (= (:old-fact token) old-fact))
                         fact)))
          (reduce
            (fn [session child]
              (right-activate-join-node session (:path child) token))
            $
            (:successors (get-in session node-path))))))

(defn- get-alpha-nodes-for-fact [session alpha-node id attr value root?]
  (if root?
    (reduce
      (fn [nodes child]
        (into nodes (get-alpha-nodes-for-fact session child id attr value false)))
      #{}
      (:children alpha-node))
    (let [value (case (:test-field alpha-node)
                  :id id
                  :attr attr
                  :value value)]
      (when (= value (:test-value alpha-node))
        (reduce
          (fn [nodes child]
            (into nodes (get-alpha-nodes-for-fact session child id attr value false)))
          #{(:path alpha-node)}
          (:children alpha-node))))))

(defn- upsert-fact [session id attr value node-paths]
  (let [id+attr [id attr]
        fact (->Fact id attr value)]
    (if-let [existing-node-paths (get-in session [:id-attr-nodes id+attr])]
      (as-> session $
            ;; retract any facts from nodes that the new fact wasn't inserted in
            (reduce
              (fn [session node-path]
                (if (not (contains? node-paths node-path))
                  (let [node (get-in session node-path)
                        old-fact (get-in node [:facts id attr])]
                    (assert old-fact)
                    (right-activate-alpha-node session node-path (->Token old-fact :retract nil)))
                  session))
              $
              existing-node-paths)
            ;; update or insert facts, depending on whether the node already exists
            (reduce
              (fn [session node-path]
                (if (contains? existing-node-paths node-path)
                  (let [node (get-in session node-path)
                        old-fact (get-in node [:facts id attr])]
                    (assert old-fact)
                    (right-activate-alpha-node session node-path (->Token fact :update old-fact)))
                  (right-activate-alpha-node session node-path (->Token fact :insert nil))))
              $
              node-paths))
      (reduce
        (fn [session node-path]
          (right-activate-alpha-node session node-path (->Token fact :insert nil)))
        session
        node-paths))))

;; public

(defn add-rule [session rule]
  (let [conditions (:conditions rule)
        conditions (assoc-in conditions [(dec (count conditions)) :rule-name] (:name rule))
        rule-path [:rule-fns (:name rule)]]
    (when (get-in session rule-path)
      (throw (ex-info (str (:name rule) " already exists in session") {})))
    (-> (reduce add-condition session conditions)
        (dissoc :mem-node-path) ;; added temporarily by add-condition
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
  (->Session (map->AlphaNode {:path nil
                              :test-field nil
                              :test-value nil
                              :children []
                              :successors []
                              :facts {}})
             {}
             {}))

(defn insert
  ([session id attr->value]
   (reduce-kv (fn [session attr value]
                (insert session id attr value))
              session attr->value))
  ([session id attr value]
   (->> (get-alpha-nodes-for-fact session (:root-node session) id attr value true)
        (upsert-fact session id attr value))))

(defn retract! [fact])

