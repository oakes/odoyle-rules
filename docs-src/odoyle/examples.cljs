(ns odoyle.examples
  (:require [odoyle.rules :as o]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            dynadoc.core)
  (:require-macros [dynadoc.example :refer [defexamples]]))

(st/instrument)
(st/unstrument 'odoyle.rules/insert)

(def rules
  (o/ruleset
    {::get-todo
     [:what
      [id :todo/text text]
      [id :todo/done done]]}))

(def empty-session (reduce o/add-rule (o/->session) rules))

(def session
  (-> empty-session
      (o/insert 1 {:todo/text "Make a rules engine"
                   :todo/done true})
      (o/insert 2 {:todo/text "Get a social life"
                   :todo/done false})))

(defexamples odoyle.rules/insert
  [{:doc "Insert facts separately"
    :with-focus [focus (-> session
                           (insert 1 :todo/text "Wash the car")
                           (insert 1 :todo/done false)
                           (insert 2 :todo/text "Buy groceries")
                           (insert 2 :todo/done false)
                           query-all)]}
   (let [session odoyle.examples/empty-session]
     focus)]
  [{:doc "Insert facts batched by id"
    :with-focus [focus (-> session
                           (insert 1 {:todo/text "Wash the car"
                                      :todo/done false})
                           (insert 2 {:todo/text "Buy groceries"
                                      :todo/done false})
                           query-all)]}
   (let [session odoyle.examples/empty-session]
     focus)])

(defexamples odoyle.rules/query-all
  [{:doc "Query all the facts from the session"
    :with-focus [focus (query-all session)]}
   (let [session odoyle.examples/session]
     focus)]
  [{:doc "Query the matches for a rule"
    :with-focus [focus (query-all session ::get-todo)]}
   (let [session odoyle.examples/session]
     focus)])

