# Using O'Doyle Rules as a poor man's DataScript

You can get really far using "just a big stupid hash map" to hold your app's state. When it finally becomes untenable, many people recommend [DataScript](https://github.com/tonsky/datascript). It's like a hash map with advanced Datalog queries. DATA!

Here's a hot take: You can build the same queries with [O'Doyle Rules](https://github.com/oakes/odoyle-rules), even though *it has no query language at all!* A rules engine can do this without a special embedded language, while giving you reactivity for free.

This example was inspired by [a question I got](https://github.com/oakes/odoyle-rules/issues/3) about querying hierrarchialchical data, which is difficult in part because spelling that word always gives me a minor brain seizure.

Let's use the example of todos with sub-todos. If we start with a flat list of todos:

```clojure
(def todos
  [{:db/id 1
    ::text "Clean the car"
    ::sub-todo-ids [2 3]}
   {:db/id 2
    ::text "Wash the windshield"
    ::sub-todo-ids []}
   {:db/id 3
    ::text "Clean the interior"
    ::sub-todo-ids [4 5]}
   {:db/id 4
    ::text "Vacuum the floor"
    ::sub-todo-ids []}
   {:db/id 5
    ::text "Wipe down the dashboard"
    ::sub-todo-ids []}])
```

It would be nice to somehow query our state and get back something like this:

```clojure
{:db/id 1
 :text "Clean the car"
 :sub-todos [{:db/id 2
              :text "Wash the windshield"
              :sub-todos []}
             {:db/id 3
              :text "Clean the interior"
              :sub-todos [{:db/id 4
                           :text "Vacuum the floor"
                           :sub-todos []}
                          {:db/id 5
                           :text "Wipe down the dashboard"
                           :sub-todos []}]}]}
```

## DataScript solution

In DataScript, these sorts of recursive queries are really nice:

```clojure
(require '[datascript.core :as d])

(def schema
  {::text {:db/cardinality :db.cardinality/one}
   ::sub-todo-ids {:db/cardinality :db.cardinality/many}})

(def db (d/db-with (d/empty-db schema) todos))

(d/pull db
  '[[:db/id]
    [::text :as :text]
    {[::sub-todo-ids :as :sub-todos] ...}]
  1)

;; {:db/id 1, :text "Clean the car", :sub-todos [{:db/id 2, :text "Wash the windshield"} {:db/id 3, :text "Clean the interior", :sub-todos [{:db/id 4, :text "Vacuum the floor"} {:db/id 5, :text "Wipe down the dashboard"}]}]}
```

Damn, that's...pretty elegant! But like with all terse, declarative DSLs, the downside always comes when you need to tweak things. I'll give an example later, but first we'll look at how we'd do this in O'Doyle.

I'll warn you upfront: it will be slightly uglier, in the same sense that an opossum is slightly uglier than a '92 Cindy Crawford. But with O'Doyle, as with an opossum, you will have better chances of success.

## O'Doyle Rules solution

In a rules engine, the idea of a query is flipped inside out. You have to ask what rules you should write to build the result you want. Once you've done that, the engine will continuously re-run the rules to keep your result up-to-date.

First, we need to create some kind of "index" that allows us to easily map ids to todo items. It should look like this:

```clojure
{1 {:id 1 :text "Clean the car"}
 2 {:id 2 :text "Wash the windshield"}
 3 {:id 3 :text "Clean the interior"}
 4 {:id 4 :text "Vacuum the floor"}
 5 {:id 5 :text "Wipe down the dashboard"}}
```

We can do this by creating a derived fact. Here's one way to do it:

```clojure
(require '[odoyle.rules :as o])

(def rules
  (o/ruleset
    {::todo
     [:what
      [id ::text text]
      :then-finally
      (->> (o/query-all session ::todo)       ;; => [{:id 1 :text "Clean the car"} {:id 2 "Wash the windshield"} ,,,]
           (reduce #(assoc %1 (:id %2) %2) {})    ;; => {1 {:id 1 :text "Clean the car"} 2 {:id 2 "Wash the windshield"} ,,,}
           (o/insert session ::todos ::by-id) ;; insert it as a new derived fact
           o/reset!)]}))
```

What we're doing is querying the rule's *own matches* in its `:then-finally` block, and creating a new fact based on it. Whenever any todo is inserted or retracted, this derived fact will update as well. See the main README of this project for more about derived facts.

Then, we write a rule that receives the `sub-todo-ids` of a todo item, along with the derived fact we just made, and creates a new fact that contains the actual sub-todos:

```clojure
(def rules
  (o/ruleset
    {,,,

     ::update-sub-todos
     [:what
      [id ::sub-todo-ids sub-todo-ids]
      [::todos ::by-id id->todo]
      :then                                       ;; when any todo is inserted
      (->> (mapv id->todo sub-todo-ids)           ;; make a vector of its sub-todos
           (o/insert session id ::sub-todos)  ;; insert it as a new derived fact
           o/reset!)]}))
```

So if your `::sub-todo-ids` is `[2 3]`, your `::sub-todos` will be `[{:id 2 :text "Wash the windshield"} {:id 3 :text "Clean the interior"}]`.

Now for the cool part. To make the todos contain their sub-todos, we add one line:

```clojure
(def rules
  (o/ruleset
    {::todo
     [:what
      [id ::text text]
      [id ::sub-todos sub-todos {:then not=}] ;; bring in the sub-todos!
      :then-finally
      (->> (o/query-all session ::todo)
           (reduce #(assoc %1 (:id %2) %2) {})
           (o/insert session ::todos ::by-id)
           o/reset!)]

     ,,,}))
```

Without the `{:then not=}`, the rules would just continuously cause each other to trigger. What it means is, "only trigger `:then` and `:then-finally` blocks if the new value of `sub-todos` is not equal to the old value". The rules will keep calling each other until there is no more work to do.

Finally, we'll make a `::root-todo` rule that makes it easy to query the first todo:

```clojure
(def rules
  (o/ruleset
    {,,,

     ::root-todo
     [:what
      [1 ::text text]
      [1 ::sub-todos sub-todos]]}))
```

And now, we try it out:

```clojure
(defn init [session]
  (-> (reduce (fn [session todo]
                (o/insert session (:db/id todo) todo))
        session todos)
      (o/insert ::todos ::by-id {}) ;; insert an empty ::by-id so ::update-sub-todos can fire the first time
      o/fire-rules))

(def session (init (reduce o/add-rule (o/->session) rules)))

(first (o/query-all session ::root-todo))

;; {:text "Clean the car", :sub-todos [{:id 2, :text "Wash the windshield", :sub-todos []} {:id 3, :text "Clean the interior", :sub-todos [{:id 4, :text "Vacuum the floor", :sub-todos []} {:id 5, :text "Wipe down the dashboard", :sub-todos []}]}]}
```

Yeah...obviously a lot more manual effort than that beautiful DataScript query we made earlier. But there are some distinct advantages to "building your own database" with a rules engine. In the spirit of honesty, though, I'll start with the downsides.

## Disadvantages of O'Doyle's approach

**Verbosity** As you can see above, using O'Doyle to build complex queries will require more effort upfront. It has no internal concept of an "index" that allows you to look up data by a certain attribute, so you will need to create that yourself as a derived fact.

**Performance** O'Doyle will almost certainly be slower. The main reason is that the `::by-id` fact is going to be recreated any time a todo is inserted or retracted, whereas a real database is smart enough to update its index incrementally. I may figure out a way to do that with O'Doyle eventually, but for now, don't use this technique if you're dealing with a ton of data.

## Advantages of O'Doyle's approach

**Reactivity** With O'Doyle, the query you built isn't just a query. You can run arbitrary code whenever your todos are updated, somewhat like a "materialized view". Just put arbitrary code in a rule's `:then` or `:then-finally` block (see the main README to understand the distinction).

There isn't a fine-grained way to react to changes in a DataScript query's results; see the [open issue](https://github.com/tonsky/datascript/issues/132) about this. As a result, people try [combining it](https://github.com/mpdairy/posh) with other libraries to claw their way back to reactivity.

**Tweakability** All is fine and dandy with high-level DSLs, until you need to make subtle changes that the DSL makes difficult or impossible. With O'Doyle, the guts of your database are exposed to you, and your "query language" is just normal Clojure code.

For example, what if we decide later that we want to make the sub todos store their parent todo's id, instead of the parent todos storing their sub todo ids? That might make more sense, because it prevents a todo from having multiple parents:

```clojure
(def todos
  [{:db/id 1
    ::text "Clean the car"
    ::parent-id nil}
   {:db/id 2
    ::text "Wash the windshield"
    ::parent-id 1}
   {:db/id 3
    ::text "Clean the interior"
    ::parent-id 1}
   {:db/id 4
    ::text "Vacuum the floor"
    ::parent-id 3}
   {:db/id 5
    ::text "Wipe down the dashboard"
    ::parent-id 3}])
```

In O'Doyle, this just turns into a Clojure problem: How do I convert those `::parent-id`s into `::sub-todo-ids`, so the rest of the code can remain unchanged? Make another rule, of course:

```clojure
(def rules
  (o/ruleset
    {,,,

     ::update-sub-todo-ids
     [:what
      [id ::parent-id parent-id]
      :then-finally
      (let [todos (o/query-all session ::update-sub-todo-ids) ;; => [{:id 2 :parent-id 1} {:id 3 :parent-id 1} ,,,]
            todos-by-parent (group-by :parent-id todos)]          ;; => {1 [{:id 2 :parent-id 1} {:id 3 :parent-id 1}]
        (->> todos                                                ;;     3 [{:id 4 :parent-id 3} {:id 5 :parent-id 3}]
             (reduce                                              ;;     ,,,}
               (fn [session {:keys [id]}]
                 (->> (mapv :id (todos-by-parent id))             ;; => [2 3]
                      (o/insert session id ::sub-todo-ids)))
               session)
             o/reset!))]}))
```

With just the addition of that rule, the new `todos` can be inserted, and you'll get the same query result from `::root-todo` as before! See [odoyle.cljc](odoyle.cljc) and [odoyle_alt.cljc](odoyle_alt.cljc) for the full code of the original and adjusted examples above.

Can we make this adjustment with our DataScript query? Probably, but I don't know how :D This new `::update-sub-todo-ids` rule isn't necessarily pretty or easy to understand, but at least your problem is now simply a Clojure problem, not a "how do I bend this special query language to do what I need it to do" problem.

And that's a good problem to have.
