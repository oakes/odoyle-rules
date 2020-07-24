[![Clojars Project](https://img.shields.io/clojars/v/net.sekao/odoyle-rules.svg)](https://clojars.org/net.sekao/odoyle-rules)

O'Doyle does indeed rule. And you will, too, when you use O'Doyle Rules, a rules engine for Clojure and ClojureScript. Stop being one of those jabronis that don't rule things. When I was a kid in Virginia our teacher tried to teach us the names of cities in our state by letting us be the "rulers" of them. My buddy Roger said he ruled Richmond. I said I ruled Chicago because I was a Bulls fan and didn't understand geography. I don't recall why I'm telling you this.

## Documentation

* [API docs](https://oakes.github.io/odoyle-rules/)
* The [dungeon crawler](https://github.com/oakes/play-cljc-examples/tree/master/dungeon-crawler) game (see [this file](https://github.com/oakes/play-cljc-examples/blob/master/dungeon-crawler/src/dungeon_crawler/session.cljc))
* The screencast: https://www.youtube.com/watch?v=6_mDiH5_hSc
* The tutorial below

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
* Clara supports [accumulators](https://www.clara-rules.org/docs/accumulators/) for gathering multiple facts together for use in a rule.
* Clara is faster. I'm still working through the '95 Doorenbos paper so my optimizations aren't even in this century yet.

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

The most important part of a rule is the `:what` block, which specifies what tuples must exist for the rule to fire. The key is that you can create a *binding* in the id or value column by supplying a symbol, like `tt` above. When the rule fires, the `then` block is executed, which has access to the bindings you created.

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
     ::get-player
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

(println (o/query-all @*session ::get-player))
;; => [{:x 20, :y 15}]
```

You can also query from inside a rule with the special `*session*` dynamic var:

```clj
(def rules
  (o/ruleset
    {::get-player
     [:what
      [::player ::x x]
      [::player ::y y]]
     ::print-player-when-time-updates
     [:what
      [::time ::total tt]
      :then
      (println "Query from inside rule:" (o/query-all o/*session* ::get-player))]}))

(swap! *session
  (fn [session]
    (-> (reduce o/add-rule (o/->session) rules)
        (o/insert ::player {::x 20 ::y 15}) ;; notice this short-hand way of inserting multiple things with the same id
        (o/insert ::time ::total 100)
        o/fire-rules)))
```

## Avoiding infinite loops

Imagine you want to move the player's position based on its current position. So instead of just using the total time, maybe we want to add the delta time to the player's latest `::x` position:

```clj
(def rules
  (o/ruleset
    {::get-player
     [:what
      [::player ::x x]
      [::player ::y y]]
     ::move-player
     [:what
      [::time ::delta dt]
      [::player ::x x {:then false}] ;; don't run the :then block if only this is updated!
      :then
      (o/insert! ::player ::x (+ x dt))]}))

(swap! *session
  (fn [session]
    (-> (reduce o/add-rule (o/->session) rules)
        (o/insert ::player {::x 20 ::y 15})
        (o/insert ::time {::total 100 ::delta 0.1})
        o/fire-rules)))

(println (o/query-all @*session ::get-player))
;; => [{:x 20.1, :y 15}]
```

The `{:then false}` option tells O'Doyle to not run the `:then` block if that tuple is updated. If you don't include it, you'll get a StackOverflowException because the rule will cause itself to fire in an infinite loop. If all tuples in the `:what` block have `{:then false}`, it will never fire.

Another way to avoid infinite loops is to not include the data you're modifying in your `:what` block at all. Instead, you could retrieve it by querying from your `:then` block with `(o/query-all o/*session* ::get-player)` and pulling out the data you want.

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

## Joins and advanced queries

Instead of the `::get-player` rule, we could make a more generic "getter" rule that works for any id:

```clj
::get-character
[:what
 [id ::x x]
 [id ::y y]]
```

Now, we're making a binding on the id column, and since we're using the same binding symbol ("id") in both, O'Doyle will ensure that they are equal, much like a join in SQL.

Now we can add multiple things with those two attributes and get them back in a single query:

```clj
(swap! *session
  (fn [session]
    (-> (reduce o/add-rule (o/->session) rules)
        (o/insert ::player {::x 20 ::y 15})
        (o/insert ::enemy {::x 5 ::y 5})
        o/fire-rules)))

(println (o/query-all @*session ::get-character))
;; => [{:id :examples.odoyle/player, :x 20, :y 15} {:id :examples.odoyle/enemy, :x 5, :y 5}]
```

## Generating ids

In addition to keywords like `::player`, it is likely that you'll want to generate ids at runtime. For example, if you just want to spawn random enemies, you probably don't want to create a special keyword for each one. For this reason, `insert` allows you to just pass arbitrary integers as ids:

```clj
(swap! *session
  (fn [session]
    (o/fire-rules
      (reduce (fn [session id]
                (o/insert session id {::x (rand-int 50) ::y (rand-int 50)}))
              session
              (range 5)))))

(println (o/query-all @*session ::get-character))
;; => [{:id 0, :x 14, :y 45} {:id 1, :x 12, :y 48} {:id 2, :x 48, :y 25} {:id 3, :x 4, :y 25} {:id 4, :x 39, :y 0}]
```

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
Error when checking attribute :examples.odoyle/width

Syntax error (ExceptionInfo) compiling at (examples\odoyle.cljc:166:1).
-- Spec failed --------------------

  0

should satisfy

  pos?
```

## Development

* Install [the Clojure CLI tool](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)
* To run the examples in this README: `clj -A:dev`
* To run the tests: `clj -A:test`
* To install the release version: `clj -A:prod install`

## Acknowledgements

I could not have built this without the [1995 thesis paper from Robert Doorenbos](http://reports-archive.adm.cs.cmu.edu/anon/1995/CMU-CS-95-113.pdf), which describes the RETE algorithm better than I've found anywhere else. I also stole a lot of design ideas from [Clara Rules](https://github.com/cerner/clara-rules).
