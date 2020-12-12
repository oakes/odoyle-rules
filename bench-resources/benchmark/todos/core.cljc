(ns benchmark.todos.core)

(def todos
  [{:db/id 1
    ::text "My todo list"
    ::sub-todo-ids [2]}
   {:db/id 2
    ::text "Clean the car"
    ::sub-todo-ids [3 4]
    ::root true}
   {:db/id 3
    ::text "Wash the windshield"
    ::sub-todo-ids []}
   {:db/id 4
    ::text "Clean the interior"
    ::sub-todo-ids [5 6]}
   {:db/id 5
    ::text "Vacuum the floor"
    ::sub-todo-ids []}
   {:db/id 6
    ::text "Wipe down the dashboard"
    ::sub-todo-ids []}])

