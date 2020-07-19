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

(defn- check-insert-spec
  ([[attr value]]
   (check-insert-spec attr value))
  ([attr value]
   (when-let [spec (s/get-spec attr)]
     (parse spec value))))

;; private

(defrecord Fact [id attr value])
(defrecord Token [fact ;; Fact
                  kind ;; :insert, :retract, :update
                  old-fact ;; only used when updating
                  ])
(defrecord Var [field ;; :id, :attr, or :value
                sym ;; symbol
                key ;; keyword
                ])
(defrecord AlphaNode [path ;; the get-in vector to reach this node from the root
                      test-field ;; :id, :attr, or :value
                      test-value ;; anything
                      children ;; vector of AlphaNode
                      successors ;; vector of JoinNode ids
                      facts ;; map of id -> (map of attr -> Fact)
                      ])
(defrecord MemoryNode [id
                       parent-id ;; JoinNode id
                       child-id ;; JoinNode id
                       rule-name ;; keyword
                       vars ;; vector of (map of keyword -> value)
                       id-attrs ;; vector of id+attr
                       then-queue ;; vector of booleans
                       ])
(defrecord JoinNode [id
                     parent-id ;; MemoryNode id
                     child-id ;; MemoryNode id
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
(defrecord Session [alpha-node ;; AlphaNode
                    beta-nodes ;; map of int -> MemoryNode or JoinNode
                    last-id ;; last id assigned to a beta node
                    rule-fns ;; fns
                    rule-ids ;; map of rule name -> the id of the associated MemoryNode
                    id-attr-nodes ;; map of id+attr -> set of alpha node paths
                    then-nodes ;; set of MemoryNode ids that need executed
                    ])

(defn- add-to-condition [condition field [kind value]]
  (case kind
    :binding (update condition :vars conj (->Var field value (keyword value)))
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
        conditions (mapv ->condition (:body what-block))
        when-body (:body when-block)
        when-body (if (> (count when-body) 1)
                    (cons 'and when-body)
                    (first when-body))
        then-body (:body then-block)
        then-body (if when-body
                    [`(when ~when-body ~@then-body)]
                    then-body)
        vars (->> conditions
                  (mapcat :vars)
                  (mapv :sym))
        destructured-map {:keys vars}]
    [rule-name conditions destructured-map then-body]))

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

(defn- is-ancestor [session node-id1 node-id2]
  (loop [node-id node-id2]
    (if-let [parent-id (:parent-id (get-in session [:beta-nodes node-id]))]
      (if (= node-id1 parent-id)
        1
        (recur parent-id))
      -1)))

(defn- add-condition [session condition]
  (let [*alpha-node-path (volatile! [:alpha-node])
        session (update session :alpha-node add-alpha-node (:nodes condition) *alpha-node-path)
        alpha-node-path @*alpha-node-path
        *last-id (volatile! (:last-id session))
        join-node-id (vswap! *last-id inc)
        mem-node-id (vswap! *last-id inc)
        parent-mem-node-id (:mem-node-id session)
        mem-node (map->MemoryNode {:id mem-node-id
                                   :parent-id join-node-id
                                   :child-id nil
                                   :rule-name (:rule-name condition)
                                   :vars []
                                   :id-attrs []
                                   :then-queue []})
        join-node (map->JoinNode {:id join-node-id
                                  :parent-id parent-mem-node-id
                                  :child-id mem-node-id
                                  :alpha-node-path alpha-node-path
                                  :condition condition})
        session (-> session
                    (assoc-in [:beta-nodes join-node-id] join-node)
                    (assoc-in [:beta-nodes mem-node-id] mem-node))
        successor-ids (conj (:successors (get-in session alpha-node-path))
                            join-node-id)
        ;; successors must be sorted by ancestry (descendents first) to avoid duplicate rule firings
        successor-ids (sort (partial is-ancestor session) successor-ids)]
    (-> session
        (update-in alpha-node-path assoc :successors successor-ids)
        (cond-> parent-mem-node-id
                (assoc-in [:beta-nodes parent-mem-node-id :child-id] join-node-id))
        (assoc :mem-node-id mem-node-id)
        (assoc :last-id @*last-id))))

(defn- get-vars-from-fact [vars condition fact]
  (reduce
    (fn [m cond-var]
      (let [var-key (:key cond-var)
            existing-val (get m var-key)]
        (case (:field cond-var)
          :id
          (if (and (some? existing-val)
                   (not= existing-val (:id fact)))
            (reduced nil)
            (assoc m var-key (:id fact)))
          :attr
          (throw (ex-info "Attributes cannot contain vars" {}))
          :value
          (if (and (some? existing-val)
                   (not= existing-val (:value fact)))
            (reduced nil)
            (assoc m var-key (:value fact))))))
    vars
    (:vars condition)))

(defn- dissoc-vec [v index]
  (let [v1 (subvec v 0 index)
        v2 (subvec v (inc index))]
    (into (into [] v1) v2)))

(declare left-activate-memory-node)

(defn- left-activate-join-node [session node-id vars token]
  (let [join-node (get-in session [:beta-nodes node-id])
        alpha-node (get-in session (:alpha-node-path join-node))]
    (reduce
      (fn [session attr->fact]
        (reduce
          (fn [session alpha-fact]
            (if-let [new-vars (get-vars-from-fact vars (:condition join-node) alpha-fact)]
              (left-activate-memory-node session (:child-id join-node) new-vars (assoc token :fact alpha-fact))
              session))
          session
          (vals attr->fact)))
      session
      (vals (:facts alpha-node)))))

(defn- left-activate-memory-node [session node-id vars token]
  (let [{:keys [id attr] :as fact} (:fact token)
        id+attr [id attr]
        node-path [:beta-nodes node-id]
        node (get-in session node-path)
        prod-node? (:rule-name node)]
    (as-> session $
          (case (:kind token)
            :insert
            (-> $
                (update-in node-path
                           (fn [node]
                             (-> node
                                 (update :vars conj vars)
                                 (update :id-attrs conj id+attr)
                                 (cond-> prod-node?
                                         (update :then-queue conj true)))))
                (cond-> prod-node?
                        (update :then-nodes conj node-id)))
            :retract
            (let [index (.indexOf (:id-attrs node) id+attr)]
              (assert (>= index 0))
              (update-in $ node-path
                         (fn [node]
                           (-> node
                               (update :vars dissoc-vec index)
                               (update :id-attrs dissoc-vec index)
                               (cond-> prod-node?
                                       (update :then-queue dissoc-vec index))))))
            :update
            (let [index (.indexOf (:id-attrs node) id+attr)]
              (assert (>= index 0))
              (-> $
                 (update-in node-path
                            (fn [node]
                              (-> node
                                 (assoc-in [:vars index] vars)
                                 (cond-> prod-node?
                                         (assoc-in [:then-queue index] true)))))
                 (cond-> prod-node?
                         (update :then-nodes conj node-id)))))
          (if-let [join-node-id (:child-id node)]
            (left-activate-join-node $ join-node-id vars token)
            $))))

(defn- right-activate-join-node [session node-id token]
  (let [node (get-in session [:beta-nodes node-id])]
    (if-let [parent-id (:parent-id node)]
      (reduce
        (fn [session existing-vars]
          (if-let [vars (get-vars-from-fact existing-vars (:condition node) (:fact token))]
            (left-activate-memory-node session (:child-id node) vars token)
            session))
        session
        (:vars (get-in session [:beta-nodes parent-id])))
      ;; root node
      (if-let [vars (get-vars-from-fact {} (:condition node) (:fact token))]
        (left-activate-memory-node session (:child-id node) vars token)
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
            (-> $
                (update-in node-path update-in [:facts id attr]
                           (fn [old-fact]
                             (assert (= (:old-fact token) old-fact))
                             fact))))
          (reduce
            (fn [session child-id]
              (right-activate-join-node session child-id token))
            $
            (:successors (get-in session node-path))))))

(defn- trigger-then-blocks [{:keys [then-nodes] :as session}]
  (if (seq then-nodes)
    (trigger-then-blocks
      (reduce
        (fn [session node-id]
          (let [node-path [:beta-nodes node-id]
                node (get-in session node-path)
                rule-name (:rule-name node)
                rule-fn (or (get-in session [:rule-fns rule-name])
                            (throw (ex-info (str rule-name " not found") {})))]
            (reduce-kv
              (fn [session i trigger?]
                (when trigger?
                  (rule-fn (nth (:vars node) i)))
                (update-in session node-path assoc-in [:then-queue i] false))
              session
              (:then-queue node))))
        (assoc session :then-nodes #{})
        then-nodes))
    session))

(defn- get-alpha-nodes-for-fact [session alpha-node id attr value root?]
  (if root?
    (reduce
      (fn [nodes child]
        (into nodes (get-alpha-nodes-for-fact session child id attr value false)))
      #{}
      (:children alpha-node))
    (let [test-value (case (:test-field alpha-node)
                       :id id
                       :attr attr
                       :value value)]
      (when (= test-value (:test-value alpha-node))
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
        rule-fn-path [:rule-fns (:name rule)]
        session (reduce add-condition session conditions)]
    (when (get-in session rule-fn-path)
      (throw (ex-info (str (:name rule) " already exists in session") {})))
    (-> session
        (assoc-in [:rule-ids (:name rule)] (:mem-node-id session))
        (dissoc :mem-node-id) ;; assoc'ed by add-condition
        (assoc-in rule-fn-path (:rule-fn rule)))))

(defmacro ruleset [rules]
  (reduce
    (fn [v [rule-name conditions fn-arg fn-body]]
      (conj v `(->Rule ~rule-name
                       (mapv map->Condition ~conditions)
                       (fn [~fn-arg] ~@fn-body))))
    []
    (mapv ->rule (parse ::rules rules))))

(defn ->session []
  (map->Session
    {:alpha-node (map->AlphaNode {:path nil
                                  :test-field nil
                                  :test-value nil
                                  :children []
                                  :successors []
                                  :facts {}})
     :beta-nodes {}
     :last-id -1
     :rule-fns {}
     :rule-ids {}
     :id-attr-nodes {}
     :then-nodes #{}}))

(s/def ::session #(instance? Session %))

(s/def ::insert-args
  (s/or
    :three (s/cat :session ::session
                  :id qualified-keyword?
                  :attr->value (s/map-of qualified-keyword? any?))
    :four (s/cat :session ::session
                 :id qualified-keyword?
                 :attr qualified-keyword?
                 :value any?)))

(s/fdef insert
  :args (s/and
          ::insert-args
          (s/conformer
            (fn [[kind args :as parsed-args]]
              (case kind
                :three (some check-insert-spec (:attr->value args))
                :four (check-insert-spec (:attr args) (:value args)))
              parsed-args))))

(defn insert
  ([session id attr->value]
   (reduce-kv (fn [session attr value]
                (insert session id attr value))
              session attr->value))
  ([session id attr value]
   (->> (get-alpha-nodes-for-fact session (:alpha-node session) id attr value true)
        (upsert-fact session id attr value)
        trigger-then-blocks)))

(s/fdef retract
  :args (s/cat :session ::session
               :id qualified-keyword?
               :attr qualified-keyword?))

(defn retract [session id attr]
  (let [id+attr [id attr]
        node-paths (get-in session [:id-attr-nodes id+attr])]
    (when-not node-paths
      (throw (ex-info (str id+attr " not in session") {})))
    (reduce
      (fn [session node-path]
        (let [node (get-in session node-path)
              fact (get-in node [:facts id attr])]
          (right-activate-alpha-node session node-path (->Token fact :retract nil))))
      session
      node-paths)))

(s/fdef query-all
  :args (s/cat :session ::session
               :rule-name qualified-keyword?))

(defn query-all [session rule-name]
  (let [rule-id (or (get-in session [:rule-ids rule-name])
                      (throw (ex-info (str rule-name " not in session") {})))
        rule (get-in session [:beta-nodes rule-id])]
    (:vars rule)))

(s/fdef query
  :args (s/cat :session ::session
               :rule-name qualified-keyword?))

(defn query [session rule-name]
  (last (query-all session rule-name)))

