# Odoyle's Rules Cookbook

## Initializing values

Suppose we have an aggregate:

```clojure
(o/ruleset
  {#_...
   ::agregate-things
   [:what 
    [thing-id :thing/name  thing-name]
    [thing-id :thing/owner person-id]
    :then-finally
    (->> (o/rules o/*session* ::aggregate-things)
         (group-by :thing-id)
         (o/insert o/*session* :global :domain/things)
         (o/reset!))]})
```

The trouble is that `:thing/owner` may be an attribute of some things, but not all of them. We want to aggregate all the things.

The solution is to use a rule that initializes a `nil` value for `:thing/owner`:

```clojure
(o/ruleset
  {#_...
   ::initialize-thing-owner
   [:what [_ :thing/id id]
    :when (not (o/contains? o/*session* id :thing/owner))
    :then (o/insert! id :thing/owner nil)]})
```

The value may not always be nil. It may also often be 0 or another neutral value. Note that `{:then not=}` prevents the rule from firing more than once. (However, if `[id :thing/id id]` is used, this will not work.)

## Build a Query System

Instead of gathering the data required for your rules, shaping it into the form you need it, and inserting it with `o/insert`, you can use rules to make the process automatic.

For example:

```clojure
(ns app.rules
  (:require [odoyle.rules :as o]
            [app.thing.db :refer [fetch-thing]])) 
            ;; fetch-thing queries the db
            ;; which returns a namespaced map
                                                   
(def rules
  (o/ruleset 
    {::fetch-thing 
     [:what [_ :thing/id id {:then not=}]
     [:then (o/insert! id (fetch-thing id))]]}))
```

Now, instead of calling `fetch-thing` and then `o/insert` from the outside, you can use `(o/insert <some-id> :thing/id <some-id>)`, and the rule will fetch for you.

The power of this approach is that O'Doyle rules becomes a query engine.

Suppose that you have `thing/owner` attribute, which is a set of the things a person owns. Combining two fetchers reveals the power of this approach:

```clojure
(ns app.rules
  (:require [odoyle.rules :as o]
            [app.person.db :refer [fetch-person]]
            [app.thing.db :refer [fetch-thing]])) 
            
(def rules
  (o/ruleset 
    {::fetch-thing
     [:what [_ :thing/id id {:then not=}]
      :then                                     ;; fetch-thing returns a namespaced map
      (o/insert! id (fetch-thing id))]          ;; `{:thing/name "name", :thing/owner 123, #_...}`

     ::fetch-person 
     [:what [_ :person/id id {:then not=}]      
      :then (o/insert! id (fetch-person-id))]   ;; fetch-person returns a namespaced map
      
     ::person-owner-link
     [:what [_ :thing/owner person-id {:then not=}]
      :then (o/insert! person-id :person/id person-id)]}))
```

The `::person-owner-link` links `:thing/owner` to `:person/id`. The call to `fetch-thing` returns a map with `:thing/owner`. This is inserted into the session.

`::person-owner-link` then fires, inserting a `:person/id`. This in turn triggers `::fetch-person` to query the database for all the `:person` information, and insert it into the session.

By providing a single id, all related domain entities can be fetched, whether from a database or a service.


## Splitting rule sets by namespaces

If you have a number of different rulesets, you can split them by namespaces, and then merge them into a single ruleset.

For example:

```clojure
(ns app.rules
  (:require [odoyle.rules :as o]
            [app.domain-a.rules :as a]
            [app.domain-b.rules :as b]))

(def base-rules
  (o/ruleset
    {::some-rule ...}))

(def ruleset
  (reduce into [base-rules a/ruleset b/ruleset]))
```
