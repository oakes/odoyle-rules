(ns todos.core)

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

(def todos-alt
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

