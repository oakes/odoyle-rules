(ns examples.odoyle
  (:require [odoyle.rules :as o]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]))

(st/instrument)

;; example 1

(def rules
  (o/ruleset
    {::print-time
     [:what
      [::time ::total tt]
      :then
      (println tt)]}))

(def *session
  (atom (reduce o/add-rule (o/->session) rules)))

(swap! *session
  (fn [session]
    (-> session
        (o/insert ::time ::total 100)
        o/fire-rules)))

;; example 2

(def rules
  (o/ruleset
    {::move-player
     [:what
      [::time ::total tt]
      :then
      (o/insert! ::player ::x tt)]}))

(swap! *session
  (fn [session]
    (-> (reduce o/add-rule (o/->session) rules)
        (o/insert ::player ::x 20)
        (o/insert ::player ::y 15)
        o/fire-rules)))

;; example 3

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
        (o/insert ::player {::x 20 ::y 15})
        (o/insert ::time ::total 100)
        o/fire-rules)))

(println (o/query-all @*session ::get-player))

;; example 4

(def rules
  (o/ruleset
    {::get-player
     [:what
      [::player ::x x]
      [::player ::y y]]
     ::move-player
     [:what
      [::time ::delta dt]
      [::player ::x x {:then false}]
      :then
      (o/insert! ::player ::x (+ x dt))]}))

(swap! *session
  (fn [session]
    (-> (reduce o/add-rule (o/->session) rules)
        (o/insert ::player {::x 20 ::y 15})
        (o/insert ::time {::total 100 ::delta 0.1})
        o/fire-rules)))

(println (o/query-all @*session ::get-player))

;; example 5

(defn on-window-resize [width height]
  (swap! *session
         (fn [session]
           (-> session
               (o/insert ::window {::width width ::height height})
               o/fire-rules))))

(def rules
  (o/ruleset
    {::get-player
     [:what
      [::player ::x x]
      [::player ::y y]]
     ::stop-player
     [:what
      [::player ::x x]
      [::window ::width window-width]
      :when
      (> x window-width)
      :then
      (o/insert! ::player ::x window-width)]}))

(swap! *session
  (fn [session]
    (-> (reduce o/add-rule (o/->session) rules)
        (o/insert ::player {::x 2000 ::y 15})
        o/fire-rules)))

(on-window-resize 800 600)

(println (o/query-all @*session ::get-player))

;; example 6

(def rules
  (o/ruleset
    {::get-character
     [:what
      [id ::x x]
      [id ::y y]]}))

(swap! *session
  (fn [session]
    (-> (reduce o/add-rule (o/->session) rules)
        (o/insert ::player {::x 20 ::y 15})
        (o/insert ::enemy {::x 5 ::y 5})
        o/fire-rules)))

(println (o/query-all @*session ::get-character))

;; example 7

(reset! *session (reduce o/add-rule (o/->session) rules))

(swap! *session
  (fn [session]
    (o/fire-rules
      (reduce (fn [session id]
                (o/insert session id {::x (rand-int 50) ::y (rand-int 50)}))
              session
              (range 5)))))

(println (o/query-all @*session ::get-character))

;; example 8

(s/def ::x number?)
(s/def ::y number?)
(s/def ::width (s/and number? pos?))
(s/def ::height (s/and number? pos?))

(reset! *session (reduce o/add-rule (o/->session) rules))

(swap! *session
  (fn [session]
    (-> session
        (o/insert ::player {::x 20 ::y 15 ::width 10 ::height 15})
        o/fire-rules)))
