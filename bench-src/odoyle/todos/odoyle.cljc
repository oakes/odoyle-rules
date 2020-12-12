(ns odoyle.todos.odoyle
  (:require [odoyle.rules :as o #?(:clj :refer :cljs :refer-macros) [ruleset]]
            [odoyle.todos.core :as core]))

(def rules
  (o/ruleset
    {::todo
     [:what
      [id ::core/text text]
      [id ::sub-todos sub-todos {:then not=}]
      :then-finally
      (->> (o/query-all o/*session* ::todo)
           (reduce #(assoc %1 (:id %2) %2) {})
           (o/insert! ::todos ::by-id))]

     ::root-todo
     [:what
      [1 ::core/text text]
      [1 ::sub-todos sub-todos]]

     ::update-sub-todos
     [:what
      [id ::core/sub-todo-ids sub-todo-ids]
      [::todos ::by-id id->todo]
      :then
      (->> (mapv id->todo sub-todo-ids)
           (o/insert! id ::sub-todos))]}))

(defn add-facts [session]
  (-> (reduce (fn [session todo]
                (o/insert session (:db/id todo) todo))
        session core/todos)
      (o/insert ::todos ::by-id {})
      o/fire-rules))

(def initial-session
  (add-facts (reduce o/add-rule (o/->session) rules)))

(defn query [session]
  (o/query-all session ::root-todo))

(defn run []
  (query initial-session))

