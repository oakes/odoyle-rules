(ns todos.odoyle
  (:require [odoyle.rules :as o]
            [todos.core :as core]))

(def rules
  (o/ruleset
    {::todo
     [:what
      [id ::core/text text]
      [id ::sub-todos sub-todos {:then not=}]
      :then-finally
      (->> (o/query-all o/*session* ::todo)
           (reduce #(assoc %1 (:id %2) %2) {})
           (o/insert o/*session* ::todos ::by-id)
           o/reset!)]

     ::update-sub-todos
     [:what
      [id ::core/sub-todo-ids sub-todo-ids]
      [::todos ::by-id id->todo]
      :then
      (->> (mapv id->todo sub-todo-ids)
           (o/insert o/*session* id ::sub-todos)
           o/reset!)]

     ::root-todo
     [:what
      [1 ::core/text text]
      [1 ::sub-todos sub-todos]]}))

(defn init [session]
  (-> (reduce (fn [session todo]
                (o/insert session (:db/id todo) todo))
        session core/todos)
      (o/insert ::todos ::by-id {})
      o/fire-rules))

(def initial-session
  (init (reduce o/add-rule (o/->session) rules)))

(defn query [session]
  (first (o/query-all session ::root-todo)))

;; benchmark

(defn tick [session counter]
  (let [random-id (inc (mod counter (count core/todos)))
        id->todo (-> (o/query-all session ::update-sub-todos)
                     first
                     :id->todo)
        random-todo-text (:text (id->todo random-id))]
    (-> session
        (o/insert random-id ::core/text (str random-todo-text " " counter))
        o/fire-rules)))

(defn run [iterations]
  (loop [session initial-session
         counter 0]
    (if (= counter iterations)
      session
      (recur (tick session counter) (inc counter)))))

