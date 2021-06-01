[![Clojars Project](https://img.shields.io/clojars/v/net.sekao/odoyle-rules.svg)](https://clojars.org/net.sekao/odoyle-rules)

<p align="center">
  <a href="https://www.youtube.com/watch?v=sz_LTHhIGsc"><img src="odoylerules.jpg" width="175" ></a>
</p>

O'Doyle does indeed rule. And you will, too, when you use O'Doyle Rules, a rules engine for Clojure and ClojureScript. Stop being one of those jabronis that don't rule things. When I was a kid in Virginia our teacher tried to teach us the names of cities in our state by letting us be the "rulers" of them. My buddy Roger said he ruled Richmond. I said I ruled Chicago because I was a Bulls fan and didn't understand geography. I don't recall why I'm telling you this.

## Documentation

* [API docs](https://oakes.github.io/odoyle-rules/)
* See real uses of O'Doyle:
  * [Dungeon Crawler](https://github.com/oakes/play-cljc-examples/tree/master/dungeon-crawler), a game (see [this file](https://github.com/oakes/play-cljc-examples/blob/master/dungeon-crawler/src/dungeon_crawler/session.cljc))
  * [Paravim](https://github.com/oakes/Paravim), a text editor (see [this file](https://github.com/oakes/Paravim/blob/master/src/paravim/session.cljc))
  * [O'Doyle Rum](https://github.com/oakes/odoyle-rum), a library for making web UIs with O'Doyle
* Watch the talk: [O'Doyle Rules - a Clojure rules engine for the best of us](https://www.youtube.com/watch?v=XONRaJJAhpA)
* Read the tutorial below:
  - [Comparison to Clara](#comparison-to-clara)
  - [Your first rule](#your-first-rule)
  - [Updating the session from inside a rule](#updating-the-session-from-inside-a-rule)
  - [Queries](#queries)
  - [Avoiding infinite loops](#avoiding-infinite-loops)
  - [Conditions](#conditions)
  - [Joins](#joins)
  - [Generating ids](#generating-ids)
  - [Derived facts](#derived-facts)
  - [Serializing a session](#serializing-a-session)
  - [Performance](#performance)
  - [Spec integration](#spec-integration)
  - [Defining rules dynamically](#defining-rules-dynamically)
  - [Development](#development)
  - [Acknowledgements](#acknowledgements)

## Comparison to Clara

O'Doyle is different than [Clara](https://www.clara-rules.org/) in a few ways.

Advantages compared to Clara:

* O'Doyle stores data in id-attribute-value tuples like `[::player ::health 10]` whereas Clara (by default) uses Clojure records. I think storing each key-value pair as a separate fact leads to a much more flexible system.
* O'Doyle has built-in support for updating facts. You don't even need to explicitly do it; simply inserting a fact with an existing id + attribute combo will cause the old fact to be removed. This is only possible because of the aforementioned use of tuples.
* O'Doyle provides a simple `ruleset` macro that defines your rules from a map of plain data. Clara's `defrule` macro creates a global var that is implicitly added to a session. I tried to solve that particular problem with my [clarax](https://github.com/oakes/clarax) library but with O'Doyle it's even cleaner.
* O'Doyle makes no distinction between rules and queries -- all rules are also queries. Clara has a separate `defquery` macro for making queries, which means potential duplication since queries can often be the same as the "left hand side" of a rule.
* O'Doyle has nice spec integration (see below).

Disadvantages compared to Clara:

* Clara supports [truth maintenance](https://www.clara-rules.org/docs/truthmaint/), which can be a very useful feature in some domains.
* Clara is probably faster out of the box (but check out the "Performance" section below).

The design of O'Doyle is almost a carbon copy of my Nim rules engine, [pararules](https://github.com/paranim/pararules).

## Your first rule

Let's start by just making a rule that prints out a timestamp whenever it updates:

```clj
(require '[odoyle.rules :as o])

(def rules
  (o/ruleset
    {::print-time
     [:what
      [::time ::total tt]
      :then
      (println tt)]}))

;; create session and add rule
(def *session
  (atom (reduce o/add-rule (o/->session) rules)))
```

The most important part of a rule is the `:what` block, which specifies what tuples must exist for the rule to fire. The key is that you can create a *binding* in the id or value column by supplying a symbol, like `tt` above. When the rule fires, the `:then` block is executed, which has access to the bindings you created.

You can then insert the time value:

```clj
(swap! *session
  (fn [session]
    (-> session
        (o/insert ::time ::total 100)
        o/fire-rules)))
```

The nice thing is that, if you insert something whose id + attribute combo already exists, it will simply replace it.

## Updating the session from inside a rule

Now imagine you want to make the player move to the right every time the frame is redrawn. Your rule might look like this:

```clj
(def rules
  (o/ruleset
    {::move-player
     [:what
      [::time ::total tt]
      :then
      (o/insert! ::player ::x tt)]}))
```

As an aside, you can also insert from inside a rule like this:

```clj
(def rules
  (o/ruleset
    {::move-player
     [:what
      [::time ::total tt]
      :then
      (-> o/*session*
          (o/insert ::player ::x tt)
          o/reset!)]}))
```

The `*session*` dynamic var will have the current value of the session, and `reset!` will update it so it has the newly-inserted value. This is nice if you want to thread a lot of calls together, or if you want to write code that works the same both inside and outside of the rule.

## Queries

Updating the player's `::x` attribute isn't useful unless we can get the value externally to render it. To do this, make another rule that binds the values you would like to receive:

```clj
(def rules
  (o/ruleset
    {::move-player
     [:what
      [::time ::total tt]
      :then
      (o/insert! ::player ::x tt)]

     ::player
     [:what
      [::player ::x x]
      [::player ::y y]]}))
``` 

As you can see, rules don't need a `:then` block if you're only using them to query from the outside. In this case, we'll query it externally and get back a vector of maps whose fields have the names you created as bindings:

```clj
(swap! *session
  (fn [session]
    (-> session
        (o/insert ::player ::x 20)
        (o/insert ::player ::y 15)
        o/fire-rules)))

(println (o/query-all @*session ::player))
;; => [{:x 20, :y 15}]
```

## Avoiding infinite loops

Imagine you want to move the player's position based on its current position. So instead of just using the total time, maybe we want to add the delta time to the player's latest `::x` position:

```clj
(def rules
  (o/ruleset
    {::player
     [:what
      [::player ::x x]
      [::player ::y y]]

     ::move-player
     [:what
      [::time ::delta dt]
      [::player ::x x {:then false}] ;; don't run the :then block if only this is updated!
      :then
      (o/insert! ::player ::x (+ x dt))]}))

(reset! *session
  (-> (reduce o/add-rule (o/->session) rules)
      (o/insert ::player {::x 20 ::y 15})
      (o/insert ::time {::total 100 ::delta 0.1})
      o/fire-rules))

(println (o/query-all @*session ::player))
;; => [{:x 20.1, :y 15}]
```

The `{:then false}` option tells O'Doyle to not run the `:then` block if that tuple is updated. If you don't include it, you'll get an exception because the rule will cause itself to fire in an infinite loop. If all tuples in the `:what` block have `{:then false}`, it will never fire.

While `{:then false}` says "this fact should *never* cause this rule to trigger", you may actually want to say "this fact should *sometimes* cause this rule to trigger", such as only when the fact's new value is different than its old value. You can do this with `{:then not=}`.

Using `{:then not=}` is not a special case; this option can receive *any* function that receives two arguments, the fact's new value and old value. In the example above, that would be the value of `x`. This little feature can be used to write rules that recursively build data structures. See: [Using O'Doyle Rules as a poor man's DataScript](bench-src/todos/README.md).

## Conditions

Rules have nice a way of breaking apart your logic into independent units. If we want to prevent the player from moving off the right side of the screen, we could add a condition inside of the `:then` block of `::move-player`, but it's good to get in the habit of making separate rules.

To do so, we need to start storing the window size in the session. Wherever your window resize happens, insert the values:

```clj
(defn on-window-resize [width height]
  (swap! *session
         (fn [session]
           (-> session
               (o/insert ::window {::width width ::height height})
               o/fire-rules))))
```

Then we make the rule:

```clj
::stop-player
[:what
 [::player ::x x]
 [::window ::width window-width]
 :then
 (when (> x window-width)
   (o/insert! ::player ::x window-width))]
```

Notice that we *don't* need `{:then false}` this time, because the condition is preventing the rule from re-firing.

While the above code works, you can also put your condition in a special `:when` block:

```clj
::stop-player
[:what
 [::player ::x x]
 [::window ::width window-width]
 :when
 (> x window-width)
 :then
 (o/insert! ::player ::x window-width)]
```

You can add as many conditions as you want, and they will implicitly work as if they were combined together with `and`:

```clj
::stop-player
[:what
 [::player ::x x]
 [::window ::width window-width]
 :when
 (> x window-width)
 (pos? window-width)
 :then
 (o/insert! ::player ::x window-width)]
```

Using a `:when` block is better because it also affects the results of `query-all` -- matches that didn't pass the conditions will not be included. Also, in the future I'll probably be able to create more optimal code because it will let me run those conditions earlier in the network.

## Joins

Instead of the `::player` rule, we could make a more generic "getter" rule that works for any id:

```clj
::character
[:what
 [id ::x x]
 [id ::y y]]
```

Now, we're making a binding on the id column, and since we're using the same binding symbol ("id") in both, O'Doyle will ensure that they are equal, much like a join in SQL.

Now we can add multiple things with those two attributes and get them back in a single query:

```clj
(reset! *session
  (-> (reduce o/add-rule (o/->session) rules)
      (o/insert ::player {::x 20 ::y 15})
      (o/insert ::enemy {::x 5 ::y 5})
      o/fire-rules))

(println (o/query-all @*session ::character))
;; => [{:id :odoyle.readme/player, :x 20, :y 15} {:id :odoyle.readme/enemy, :x 5, :y 5}]
```

## Generating ids

So far our ids have been keywords like `::player`, but you can use anything as an id. For example, if you want to spawn random enemies, you probably don't want to create a special keyword for each one. Instead, you can pass arbitrary integers as ids:

```clj
(swap! *session
  (fn [session]
    (o/fire-rules
      (reduce (fn [session id]
                (o/insert session id {::x (rand-int 50) ::y (rand-int 50)}))
              session
              (range 5)))))

(println (o/query-all @*session ::character))
;; => [{:id 0, :x 14, :y 45} {:id 1, :x 12, :y 48} {:id 2, :x 48, :y 25} {:id 3, :x 4, :y 25} {:id 4, :x 39, :y 0}]
```

## Derived facts

Sometimes we want to make a rule that receives a *collection* of facts. In Clara, this is done with [accumulators](https://www.clara-rules.org/docs/accumulators/). In O'Doyle, this is done by creating facts that are derived from other facts.

If you want to create a fact that contains all characters, one clever way to do it is to run a query in the `::character` rule, and insert the result as a new fact:

```clj
(def rules
  (o/ruleset
    {::character
     [:what
      [id ::x x]
      [id ::y y]
      :then
      (->> (o/query-all o/*session* ::character)
           (o/insert o/*session* ::derived ::all-characters)
           o/reset!)]

     ::print-all-characters
     [:what
      [::derived ::all-characters all-characters]
      :then
      (println "All characters:" all-characters)]}))
```

Every time *any* character is updated, the query is run again and the derived fact is updated. When we insert our random enemies, it seems to work:

```clj
(swap! *session
  (fn [session]
    (o/fire-rules
      (reduce (fn [session id]
                (o/insert session id {::x (rand-int 50) ::y (rand-int 50)}))
              session
              (range 5)))))
;; => All characters: [{:id 0, :x 14, :y 45} {:id 1, :x 12, :y 48} {:id 2, :x 48, :y 25} {:id 3, :x 4, :y 25} {:id 4, :x 39, :y 0}]
```

But what happens if we retract one?

```clj
(swap! *session
  (fn [session]
    (-> session
        (o/retract 0 ::x)
        (o/retract 0 ::y)
        o/fire-rules)))
```

It didn't print, which means the `::all-characters` fact hasn't been updated! This is because `:then` blocks only run on insertions, not retractions. After all, if facts pertinent to a rule are retracted, the match will be incomplete, and there will be nothing to bind the symbols from the `:what` block to.

The solution is to use `:then-finally`:

```clj
::character
[:what
 [id ::x x]
 [id ::y y]
 :then-finally
 (->> (o/query-all o/*session* ::character)
      (o/insert o/*session* ::derived ::all-characters)
      o/reset!)]
```

A `:then-finally` block runs when a rule's matches are changed at all, including from retractions. This also means you won't have access to the bindings from the `:what` block, so if you want to run code on each individual match, you need to use a normal `:then` block before it.

**Important rule of thumb:** When running `query-all` inside a rule, you should *only* query the rule you are inside of, not any other rule. We can illustrate this with an example.

Let's say you want to create a derived fact that contains all characters that are within the window. We need the window dimensions, but we're using `:then-finally`, so we can't just add `[::window ::width window-width]` and `[::window ::height window-height]` to the `:what` block -- we don't have access to those bindings.

Instead, you may be tempted to do this:

```clj
(defn within? [{:keys [x y]} window-width window-height]
  (and (>= x 0)
       (< x window-width)
       (>= y 0)
       (< y window-height)))

(def rules
  (o/ruleset
    {::window
     [:what
      [::window ::width window-width]
      [::window ::height window-height]]

     ::character
     [:what
      [id ::x x]
      [id ::y y]
      :then-finally
      (let [{:keys [window-width window-height]}
            (first (o/query-all o/*session* ::window))] ;; warning: this will not be reactive!
        (->> (o/query-all o/*session* ::character)
             (filterv #(within? % window-width window-height))
             (o/insert o/*session* ::derived ::characters-within-window)
             o/reset!))]}))
```

Here, we are querying a getter rule to get the window dimensions, and using it to filter the characters. This will work initially, but if we change the window dimentions later, the `:character` rule with *not* re-run, so the `:characters-within-window` derived fact will be inaccurate.

The solution, like is often true in software, is to pull things apart that shouldn't be together:

```clj
(def rules
  (o/ruleset
    {::character
     [:what
      [id ::x x]
      [id ::y y]
      :then-finally
      (->> (o/query-all o/*session* ::character)
           (o/insert o/*session* ::derived ::all-characters)
           o/reset!)]

     ::characters-within-window
     [:what
      [::window ::width window-width]
      [::window ::height window-height]
      [::derived ::all-characters all-characters]
      :then
      (->> all-characters
           (filterv #(within? % window-width window-height))
           (o/insert o/*session* ::derived ::characters-within-window)
           o/reset!)]}))
```

First we create a derived fact holding all characters, and then in a separate rule we bring that fact in along with the window dimensions, and create the filtered fact from there. Since these are just normal facts in the `:what` block now, the rule will run when any of them are updated, just like we want it to.

## Serializing a session

To save a session to the disk or send it over a network, we need to serialize it somehow. While O'Doyle sessions are mostly pure clojure data, it wouldn't be a good idea to directly serialize them. It would prevent you from updating your rules, or possibly even the version of this library, due to all the implementation details contained in the session map after deserializing it.

Instead, it makes more sense to just serialize the *facts*. There is an arity of `query-all` that returns a vector of all the individual facts that were inserted:

```clj
(println (o/query-all @*session))
;; => [[3 :odoyle.readme/y 42] [2 :odoyle.readme/y 39] [2 :odoyle.readme/x 37] [:odoyle.readme/derived :odoyle.readme/all-characters [{:id 1, :x 46, :y 30} {:id 2, :x 37, :y 39} {:id 3, :x 43, :y 42} {:id 4, :x 6, :y 26}]] [3 :odoyle.readme/x 43] [1 :odoyle.readme/y 30] [1 :odoyle.readme/x 46] [4 :odoyle.readme/y 26] [4 :odoyle.readme/x 6]]
```

Notice that it includes the `::all-characters` derived fact that we made before. There is no need to serialize derived facts -- they can be derived later, so it's a waste of space. We can filter them out before serializing:

```clj
(def facts (->> (o/query-all @*session)
                (remove (fn [[id]]
                          (= id ::derived)))))

(spit "facts.edn" (pr-str facts))
```

Later on, we can read the facts and insert them into a new session:

```clj
(def facts (clojure.edn/read-string (slurp "facts.edn")))

(swap! *session
  (fn [session]
    (o/fire-rules
      (reduce o/insert session facts))))
```

## Performance

In any non-trivial project, you'll end up with a lot of rules that share some common tuples in their `:what` blocks, like this:

```clojure
(def rules
  (o/ruleset
    {::character
     [:what
      [id ::x x]
      [id ::y y]]

      ::move-character
      [:what
       [::time ::delta dt]
       [id ::x x {:then false}]
       [id ::y y {:then false}]
       :then
       (o/insert! id {::x (+ x dt) ::y (+ y dt)})]}))
```

Here we have a `::character` rule whose only purpose is for queries, and a `::move-character` rule that modifies it when the timestamp is updated. In both cases, there is a join on the `id` binding. Joins are not free -- they have a runtime cost, and in this case, that cost is paid twice.

In theory, this could be solved using a common optimization in rules engines calling node sharing. However, node sharing adds a lot of complexity to the codebase, and if it is automatic, it can be easily lost when subtle changes are made to a rule -- a regression that isn't easy to notice.

It turns out that a feature we've already discussed can solve this: derived facts. The above example can be rewritten like this:

```clojure
(def rules
  (o/ruleset
    {::character
     [:what
      [id ::x x]
      [id ::y y]
      :then
      (o/insert! id ::character o/*match*)]

      ::move-character
      [:what
       [::time ::delta dt]
       [id ::character ch {:then false}]
       :then
       (o/insert! id {::x (+ (:x ch) dt) ::y (+ (:y ch) dt)})]}))
```

With `*match*` we can get all the bindings in a convenient map, such as `{:id ::player, :x 10, :y 5}`. We then insert it as a derived fact, and bring it into the `::move-character` rule.

This will be faster because we now are only doing the join once, and all subsequent rules are just using the derived fact. As the number of joined tuples gets larger, the performance difference gets more and more substantial.

## Spec integration

Notice that we've been using qualified keywords a lot. What else uses qualified keywords? Spec, of course! This opens up a really cool possibility. If you have spec instrumented, and there are specs with the same name as an O'Doyle attribute, it will check your inputs when you `insert`. For example:

```clj
(require
  '[clojure.spec.alpha :as s]
  '[clojure.spec.test.alpha :as st])

(st/instrument)

(s/def ::x number?)
(s/def ::y number?)
(s/def ::width (s/and number? pos?))
(s/def ::height (s/and number? pos?))

(swap! *session
  (fn [session]
    (-> session
        (o/insert ::player {::x 20 ::y 15 ::width 0 ::height 15})
        o/fire-rules)))
```

This will produce the following error:

```
Error when checking attribute :odoyle.readme/width

Syntax error (ExceptionInfo) compiling at (odoyle\readme.cljc:166:1).
-- Spec failed --------------------

  0

should satisfy

  pos?
```

Note that as of the latest version, O'Doyle will throw an error if spec is instrumented and you try to insert an attribute that doesn't have a corresponding spec defined. Even if you are lazy and define all your specs as `any?`, this can still help to prevent typos, including the common mistake of inserting attributes with the wrong namespace qualification.

If you do *not* want O'Doyle to force attributes to all have specs defined, just call `(clojure.spec.test.alpha/unstrument 'odoyle.rules/insert)` after your `instrument` call.

## Defining rules dynamically

The `ruleset` macro gives a clean and convenient way to define rules, but it comes with the same downside that all macros have: It runs at compile time, so you can't use it to define rules "dynamically". You may want to define rules whose `:what` block is determined by information at runtime.

To do this, you can instead use the `->rule` function:

```clojure
(def rule
  (o/->rule
    ::character
    [:what
     '[id ::x x]
     '[id ::y y]
     :when
     (fn [{:keys [x y] :as match}]
       (and (pos? x) (pos? y)))
     :then
     (fn [match]
       (println "This will fire twice"))
     :then-finally
     (fn []
       (println "This will fire once"))]))

(-> (o/add-rule (o/->session) rule)
    (o/insert 1 {::x 3 ::y 1})
    (o/insert 2 {::x 5 ::y 2})
    (o/insert 3 {::x 7 ::y -1})
    o/fire-rules
    (o/query-all ::character)
    println)
;; => [{:id 1, :x 3, :y 1} {:id 2, :x 5, :y 2}]
```

As you can see, the syntax is a bit more verbose because you need to make the `fn`s explicitly. The advantage, though, is that you are no longer using a macro, so you have an opportunity to modify the `:what` block at runtime.

For example, you may want to create getter rules for a variety of different things that differ only in their id. With the `ruleset` macro, this would probably lead to a lot of duplication. Instead, you can define a function that returns a rule:

```clojure
(defn ->character-rule [id]
  (o/->rule id
    [:what
     [id ::x 'x]
     [id ::y 'y]]))

(reset! *session
  (-> (o/->session)
      (o/add-rule (->character-rule ::player))
      (o/add-rule (->character-rule ::enemy))
      (o/insert ::player {::x 20 ::y 15})
      (o/insert ::enemy {::x 5 ::y 5})
      o/fire-rules))

(println (first (o/query-all @*session ::player)))
;; => {:x 20, :y 15}
(println (first (o/query-all @*session ::enemy)))
;; => {:x 5, :y 5}
```

## Development

* Install [the Clojure CLI tool](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)
* To run the examples in this README: `clj -M:dev`
* To run the tests: `clj -M:test`
* To run a benchmark: `clj -M:bench dungeon`
* To install the release version: `clj -M:prod install`

## Acknowledgements

I could not have built this without the [1995 thesis paper from Robert Doorenbos](http://reports-archive.adm.cs.cmu.edu/anon/1995/CMU-CS-95-113.pdf), which describes the RETE algorithm better than I've found anywhere else. I also stole a lot of design ideas from [Clara Rules](https://github.com/cerner/clara-rules).
