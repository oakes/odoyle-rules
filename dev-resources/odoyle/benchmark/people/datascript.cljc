(ns odoyle.benchmark.people.datascript
  (:require
    [datascript.core :as d]
    [odoyle.benchmark.people.core :as core]))

(def schema
  {:follows {:db/valueType   :db.type/ref
             :db/cardinality :db.cardinality/many}
   :alias   {:db/cardinality :db.cardinality/many}})

(defn init []
  (d/db-with (d/empty-db schema) core/people20k))

(def initial-db (init))

(defn query [db]
  (d/q '[:find ?e ?l ?a
         :keys id last-name age
         :where [?e ::core/name "Ivan"]
                [?e ::core/last-name ?l]
                [?e ::core/age ?a]
                [?e ::core/sex :male]]
    db))

(defn bench []
  (time (query initial-db)))

