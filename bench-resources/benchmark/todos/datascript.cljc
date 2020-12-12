(ns benchmark.todos.datascript
  (:require
    [datascript.core :as d]
    [benchmark.todos.core :as core]))

(def schema
  {::core/text {:db/cardinality :db.cardinality/one}
   ::core/sub-todo-ids {:db/cardinality :db.cardinality/many}})

(defn init []
  (d/db-with (d/empty-db schema) core/todos))

(def initial-db (init))

(defn query [db]
  (d/pull db
    '[[:db/id]
      [::core/text :as :text]
      {[::core/sub-todo-ids :as :sub-todos] ...}]
    1))

(defn run []
  (query initial-db))

