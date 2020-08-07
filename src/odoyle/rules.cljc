(ns odoyle.rules
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.string :as str]))

;; parsing

(s/def ::id #(or (qualified-keyword? %) (integer? %)))
(s/def ::attr qualified-keyword?)
(s/def ::value any?)
(s/def ::what-id (s/or :binding simple-symbol? :value ::id))
(s/def ::what-attr (s/or :value ::attr))
(s/def ::what-value (s/or :binding simple-symbol? :value ::value))
(s/def ::then boolean?)
(s/def ::what-opts (s/keys :opt-un [::then]))
(s/def ::what-tuple (s/cat :id ::what-id, :attr ::what-attr, :value ::what-value, :opts (s/? ::what-opts)))
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
(defrecord Binding [field ;; :id, :attr, or :value
                    sym ;; symbol
                    key ;; keyword
                    ])
(defrecord Match [vars ;; map of binding keywords -> values from facts
                  enabled ;; boolean indicating if this match should be returned in queries
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
                       leaf-node-id ;; id of the MemoryNode at the end (same as id if this is the leaf node)
                       condition ;; Condition associated with this node
                       rule-name ;; keyword
                       matches ;; map of id+attrs -> Match
                       filter-fn ;; the :when condition in a function
                       trigger ;; boolean indicating that the :then block can be triggered
                       ])
(defrecord JoinNode [id
                     parent-id ;; MemoryNode id
                     child-id ;; MemoryNode id
                     alpha-node-path ;; the get-in vector to reach the parent AlphaNode from the root
                     condition ;; Condition associated with this node
                     id-key ;; the name of the id binding if we know it
                     old-id-attrs ;; a set of id+attr so the node can keep track of which facts are "new"
                     disable-fast-updates ;; boolean indicating it isn't safe to do fast updates
                     ])
(defrecord Condition [nodes ;; vector of AlphaNode
                      bindings ;; vector of Binding
                      opts ;; map of options
                      ])
(defrecord Rule [name ;; keyword
                 conditions ;; vector of Condition
                 rule-fn ;; fn
                 filter-fn ;; fn
                 ])
(defrecord Session [alpha-node ;; AlphaNode
                    beta-nodes ;; map of int -> MemoryNode or JoinNode
                    last-id ;; last id assigned to a beta node
                    rule-fns ;; fns
                    rule-ids ;; map of rule name -> the id of the associated MemoryNode
                    id-attr-nodes ;; map of id+attr -> set of alpha node paths
                    then-queue ;; set of (MemoryNode id, id+attrs) that need executed
                    ])

(defn- add-to-condition [condition field [kind value]]
  (case kind
    :binding (update condition :bindings conj (->Binding field value (keyword value)))
    :value (update condition :nodes conj (map->AlphaNode {:path nil
                                                          :test-field field
                                                          :test-value value
                                                          :children []
                                                          :successors []
                                                          :facts {}}))))

(defn- ->condition [{:keys [id attr value opts]}]
  (-> {:bindings [] :nodes [] :opts opts}
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
        syms (->> conditions
                  (mapcat :bindings)
                  (map :sym)
                  set
                  vec)]
    {:rule-name rule-name
     :fn-name (-> (str (namespace rule-name) "-" (name rule-name))
                  (str/replace "." "-")
                  symbol)
     :conditions conditions
     :arg {:keys syms}
     :when-body when-body
     :then-body then-body}))

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
        parent-mem-node-id (-> session :mem-node-ids last)
        mem-node (map->MemoryNode {:id mem-node-id
                                   :parent-id join-node-id
                                   :child-id nil
                                   :leaf-node-id nil
                                   :condition condition
                                   :rule-name nil
                                   :matches {}
                                   :trigger false})
        join-node (map->JoinNode {:id join-node-id
                                  :parent-id parent-mem-node-id
                                  :child-id mem-node-id
                                  :alpha-node-path alpha-node-path
                                  :condition condition
                                  :id-key nil
                                  :old-id-attrs #{}
                                  :disable-fast-updates false})
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
        (assoc :last-id @*last-id)
        ;; these are only being added temporarily
        ;; they will be removed later
        (update :mem-node-ids (fn [node-ids]
                                (if node-ids
                                  (conj node-ids mem-node-id)
                                  [mem-node-id])))
        (update :join-node-ids (fn [node-ids]
                                 (if node-ids
                                   (conj node-ids join-node-id)
                                   [join-node-id])))
        (update :bindings (fn [bindings]
                            (reduce
                              (fn [bindings k]
                                (if (contains? (:all bindings) k)
                                  (update bindings :joins conj k)
                                  (update bindings :all conj k)))
                              (or bindings
                                  {:all #{} :joins #{}})
                              (->> condition :bindings (map :key))))))))

(defn- get-vars-from-fact [vars condition fact]
  (reduce
    (fn [m cond-var]
      (let [var-key (:key cond-var)]
        (case (:field cond-var)
          :id
          (if (and (contains? m var-key)
                   (not= (get m var-key) (:id fact)))
            (reduced nil)
            (assoc m var-key (:id fact)))
          :attr
          (throw (ex-info "Attributes cannot contain vars" {}))
          :value
          (if (and (contains? m var-key)
                   (not= (get m var-key) (:value fact)))
            (reduced nil)
            (assoc m var-key (:value fact))))))
    vars
    (:bindings condition)))

(def ^:private get-id-attr (juxt :id :attr))

(declare left-activate-memory-node)

(defn- left-activate-join-node
  ([session node-id id+attrs vars token]
   (let [join-node (get-in session [:beta-nodes node-id])
         alpha-node (get-in session (:alpha-node-path join-node))]
     ;; SHORTCUT: if we know the id, only loop over alpha facts with that id
     (if-let [id (some->> join-node :id-key (get vars))]
       (reduce
         (fn [session alpha-fact]
           (left-activate-join-node session join-node id+attrs vars token alpha-fact))
         session
         (vals (get-in alpha-node [:facts id])))
       (reduce
         (fn [session attr->fact]
           (reduce
             (fn [session alpha-fact]
               (left-activate-join-node session join-node id+attrs vars token alpha-fact))
             session
             (vals attr->fact)))
         session
         (vals (:facts alpha-node))))))
  ([session join-node id+attrs vars token alpha-fact]
   (if-let [new-vars (get-vars-from-fact vars (:condition join-node) alpha-fact)]
     (let [id+attr (get-id-attr alpha-fact)
           id+attrs (conj id+attrs id+attr)
           new-token (assoc token :fact alpha-fact)
           new? (not (contains? (:old-id-attrs join-node) id+attr))]
       (left-activate-memory-node session (:child-id join-node) id+attrs new-vars new-token new?))
     session)))

(defn- left-activate-memory-node [session node-id id+attrs vars token new?]
  (let [node-path [:beta-nodes node-id]
        node (get-in session node-path)
        ;; if this insert/update fact is new
        ;; and the condition doesn't have {:then false}
        ;; let the leaf node trigger
        session (if (and new?
                         (#{:insert :update} (:kind token))
                         (-> node :condition :opts :then (not= false)))
                  (assoc-in session [:beta-nodes (:leaf-node-id node) :trigger] true)
                  session)
        node (get-in session node-path) ;; get node again since trigger may have updated
        leaf-node? (= (:id node) (:leaf-node-id node))
        ;; whether the matches in this node should
        ;; return in query results
        enabled? (boolean
                   (or (not leaf-node?)
                       (nil? (:filter-fn node))
                       ((:filter-fn node) vars)))
        ;; the id+attr of this token is the last one in the vector
        id+attr (last id+attrs)]
    (as-> session $
          (case (:kind token)
            (:insert :update)
            (-> $
                (update-in node-path assoc-in [:matches id+attrs]
                           (->Match vars enabled?))
                (cond-> (and leaf-node? (:trigger node))
                        (update :then-queue conj [node-id id+attrs]))
                (update-in [:beta-nodes (:parent-id node) :old-id-attrs]
                           conj id+attr))
            :retract
            (-> $
                (update-in node-path update :matches dissoc id+attrs)
                (update-in [:beta-nodes (:parent-id node) :old-id-attrs]
                           disj id+attr)))
          (if-let [join-node-id (:child-id node)]
            (left-activate-join-node $ join-node-id id+attrs vars token)
            $))))

(defn- right-activate-join-node [session node-id id+attr token]
  (let [node (get-in session [:beta-nodes node-id])]
    (if-let [parent-id (:parent-id node)]
      (reduce-kv
        (fn [session id+attrs {existing-vars :vars}]
          ;; SHORTCUT: if we know the id, compare it with the token right away
          (if (some->> node :id-key (get existing-vars) (not= (-> token :fact :id)))
            session
            (if-let [vars (get-vars-from-fact existing-vars (:condition node) (:fact token))]
              (left-activate-memory-node session (:child-id node) (conj id+attrs id+attr) vars token true)
              session)))
        session
        (get-in session [:beta-nodes parent-id :matches]))
      ;; root node
      (if-let [vars (get-vars-from-fact {} (:condition node) (:fact token))]
        (left-activate-memory-node session (:child-id node) [id+attr] vars token true)
        session))))

(defn- right-activate-alpha-node [session node-path token]
  (let [fact (:fact token)
        [id attr :as id+attr] (get-id-attr fact)]
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
              (if (and (= :update (:kind token))
                       (get-in session [:beta-nodes child-id :disable-fast-updates]))
                (-> session
                    (right-activate-join-node child-id id+attr (->Token (:old-fact token) :retract nil))
                    (right-activate-join-node child-id id+attr (->Token (:fact token) :insert nil)))
                (right-activate-join-node session child-id id+attr token)))
            $
            (:successors (get-in session node-path))))))

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

(def ^:private ^:dynamic *mutable-session* nil)

;; public

(def ^{:dynamic true
       :doc "Provides the current value of the session from inside a :then block.
  Normally used to allow querying the session from inside a rule."}
  *session* nil)

(s/fdef fire-rules
  :args (s/cat :session ::session))

(defn fire-rules
  "Fires :then blocks for any rules with a complete set of matches."
  [session]
  (let [then-queue (:then-queue session)]
    (if (and (seq then-queue)
             ;; don't trigger :then blocks while inside a rule
             (nil? *session*))
      (let [*trigger-queue (volatile! []) ;; a vector of fns and args to run
            session (reduce
                      (fn [session [node-id id+attrs]]
                        (let [node-path [:beta-nodes node-id]
                              node (get-in session node-path)
                              {:keys [matches rule-name]} node
                              rule-fn (or (get-in session [:rule-fns rule-name])
                                          (throw (ex-info (str rule-name " not found") {})))]
                          (when-let [{:keys [vars enabled]} (get matches id+attrs)]
                            (when enabled
                              (vswap! *trigger-queue conj [rule-fn vars])))
                          (update-in session node-path assoc :trigger false)))
                      session
                      then-queue)
            session (assoc session :then-queue #{})]
        (->> @*trigger-queue
             (reduce
               (fn [session [rule-fn args]]
                 (binding [*session* session
                           *mutable-session* (volatile! session)]
                   (rule-fn args)
                   @*mutable-session*))
               session)
             ;; recur because there may be new :then blocks to execute
             fire-rules))
      session)))

(defn add-rule
  "Adds a rule to the given session."
  [session rule]
  (when (get-in session [:rule-fns (:name rule)])
    (throw (ex-info (str (:name rule) " already exists in session") {})))
  (let [conditions (:conditions rule)
        session (reduce add-condition session conditions)
        leaf-node-id (-> session :mem-node-ids last)
        ;; the bindings (symbols) from the :what block
        bindings (:bindings session)
        ;; update all memory nodes with:
        ;; 1. the rule name
        ;; 2. the id of their leaf node
        session (reduce (fn [session mem-node-id]
                          (update-in session [:beta-nodes mem-node-id]
                                     (fn [mem-node]
                                       (assoc mem-node
                                              :rule-name (:name rule)
                                              :leaf-node-id leaf-node-id))))
                        session
                        (:mem-node-ids session))
        ;; update all join nodes with:
        ;; 1. the name of the id binding, if it exists
        ;; 2. whether to disable fast updates
        session (reduce (fn [session join-node-id]
                          (update-in session [:beta-nodes join-node-id]
                                     (fn [join-node]
                                       (assoc join-node
                                              :id-key (some (fn [{:keys [field key]}]
                                                              (when (and (= :id field)
                                                                         (contains? (:joins bindings) key))
                                                                key))
                                                            (-> join-node :condition :bindings))
                                              ;; disable fast updates for facts whose value is part of a join
                                              :disable-fast-updates (contains? (:joins bindings)
                                                                               (some (fn [{:keys [field key]}]
                                                                                       (when (= :value field)
                                                                                         key))
                                                                                     (-> join-node :condition :bindings)))))))
                        session
                        (:join-node-ids session))]
    (-> session
        (assoc-in [:beta-nodes leaf-node-id :filter-fn] (:filter-fn rule))
        (assoc-in [:rule-ids (:name rule)] leaf-node-id)
        (assoc-in [:rule-fns (:name rule)] (:rule-fn rule))
        ;; assoc'ed by add-condition
        (dissoc :mem-node-ids :join-node-ids :bindings))))

(defmacro ruleset
  "Returns a vector of rules after transforming the given map."
  [rules]
  (reduce
    (fn [v {:keys [rule-name fn-name conditions then-body when-body arg]}]
      (conj v `(->Rule ~rule-name
                       (mapv map->Condition '~conditions)
                       (fn ~fn-name [~arg] ~@then-body)
                       ~(when (some? when-body)
                          `(fn [~arg] ~when-body)))))
    []
    (mapv ->rule (parse ::rules rules))))

(defn ->session
  "Returns an empty session."
  []
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
     :then-queue #{}}))

(s/def ::session #(instance? Session %))

(s/def ::insert-args
  (s/or
    :batch (s/cat :session ::session
                  :id ::id
                  :attr->value (s/map-of ::attr ::value))
    :single (s/cat :session ::session
                   :id ::id
                   :attr ::attr
                   :value ::value)))

(defn- check-insert-spec
  ([[attr value]]
   (check-insert-spec attr value))
  ([attr value]
   (when-let [spec (s/get-spec attr)]
     (when (= ::s/invalid (s/conform spec value))
       (throw (ex-info (str "Error when checking attribute " attr "\n\n"
                            (expound/expound-str spec value))
                       {}))))))

(def ^:private insert-conformer
  (s/conformer
    (fn [[kind args :as parsed-args]]
      (case kind
        :batch (some check-insert-spec (:attr->value args))
        :single (check-insert-spec (:attr args) (:value args)))
      parsed-args)))

(s/fdef insert
  :args (s/and ::insert-args insert-conformer))

(defn insert
  "Inserts a fact into the session. Can optionally insert multiple facts with the same id."
  ([session id attr->value]
   (reduce-kv (fn [session attr value]
                (insert session id attr value))
              session attr->value))
  ([session id attr value]
   (->> (get-alpha-nodes-for-fact session (:alpha-node session) id attr value true)
        (upsert-fact session id attr value))))

(s/def ::insert!-args
  (s/or
    :batch (s/cat :id ::id
                  :attr->value (s/map-of ::attr ::value))
    :single (s/cat :id ::id
                   :attr ::attr
                   :value ::value)))

(s/fdef insert!
  :args (s/and ::insert!-args insert-conformer))

(defn insert!
  "Same as `insert` but can be used inside of a :then block."
  ([id attr->value]
   (run! (fn [[attr value]]
           (insert! id attr value))
         attr->value))
  ([id attr value]
   (if *mutable-session*
     (vswap! *mutable-session* insert id attr value)
     (throw (ex-info "This function must be called in a :then block" {})))))

(s/fdef retract
  :args (s/cat :session ::session
               :id ::id
               :attr ::attr))

(defn retract
  "Retracts the fact with the given id + attr combo."
  [session id attr]
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

(s/fdef retract!
  :args (s/cat :id ::id
               :attr ::attr))

(defn retract!
  "Same as `retract` but can be used inside of a :then block."
  [id attr]
  (if *mutable-session*
    (vswap! *mutable-session* retract id attr)
    (throw (ex-info "This function must be called in a :then block" {}))))

(s/fdef query-all
  :args (s/cat :session ::session
               :rule-name qualified-keyword?))

(defn query-all
  "Returns a vector of maps containing all the matches for the given rule."
  [session rule-name]
  (let [rule-id (or (get-in session [:rule-ids rule-name])
                    (throw (ex-info (str rule-name " not in session") {})))
        rule (get-in session [:beta-nodes rule-id])]
    (reduce-kv
      (fn [v _ {:keys [vars enabled]}]
        (if enabled
          (conj v vars)
          v))
      []
      (:matches rule))))

