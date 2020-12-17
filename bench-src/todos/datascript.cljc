(ns todos.datascript
  (:require
    [datascript.core :as d]
    [todos.core :as core]))

(def schema
  {::core/text {:db/cardinality :db.cardinality/one}
   ::core/sub-todo-ids {:db/cardinality :db.cardinality/many}})

(defn init []
  (d/db-with (d/empty-db schema) core/todos))

(def initial-db (init))

(defn query [db id]
  (d/pull db
    '[[:db/id]
      [::core/text :as :text]
      {[::core/sub-todo-ids :as :sub-todos] ...}]
    id))

;; benchmark

(defn tick [db counter]
  (let [random-id (inc (mod counter (count core/todos)))
        random-todo-text (:text (query db random-id))]
    (d/db-with db [{:db/id random-id
                    ::core/text (str random-todo-text " " counter)}])))

(defn run [iterations]
  (loop [db initial-db
         counter 0]
    (if (= counter iterations)
      db
      (recur (tick db counter) (inc counter)))))

