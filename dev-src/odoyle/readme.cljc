(ns odoyle.readme
  (:require [odoyle.rules :as o #?(:clj :refer :cljs :refer-macros) [ruleset]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [#?(:clj clojure.edn :cljs cljs.reader) :as edn]))

(st/instrument)

;; example 1

(def rules-1
  (o/ruleset
    {::print-time
     [:what
      [::time ::total tt]
      :then
      (println tt)]}))

(def *session
  (atom (reduce o/add-rule (o/->session) rules-1)))

(swap! *session
  (fn [session]
    (-> session
        (o/insert ::time ::total 100)
        o/fire-rules)))

;; example 2

(def rules-2
  (o/ruleset
    {::move-player
     [:what
      [::time ::total tt]
      :then
      (-> o/*session*
          (o/insert ::player ::x tt)
          o/reset!)]}))

(reset! *session
  (-> (reduce o/add-rule (o/->session) rules-2)
      (o/insert ::player ::x 20)
      (o/insert ::player ::y 15)
      o/fire-rules))

;; example 3

(def rules-3
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

(reset! *session
  (-> (reduce o/add-rule (o/->session) rules-3)
      (o/insert ::player {::x 20 ::y 15})
      (o/insert ::time ::total 100)
      o/fire-rules))

(println (o/query-all @*session ::get-player))

;; example 4

(def rules-4
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
      (-> o/*session*
          (o/insert ::player ::x (+ x dt))
          o/reset!)]}))

(reset! *session
  (-> (reduce o/add-rule (o/->session) rules-4)
      (o/insert ::player {::x 20 ::y 15})
      (o/insert ::time {::total 100 ::delta 0.1})
      o/fire-rules))

(println (o/query-all @*session ::get-player))

;; example 5

(defn on-window-resize [width height]
  (swap! *session
         (fn [session]
           (-> session
               (o/insert ::window {::width width ::height height})
               o/fire-rules))))

(def rules-5
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
      (-> o/*session*
          (o/insert ::player ::x window-width)
          o/reset!)]}))

(reset! *session
  (-> (reduce o/add-rule (o/->session) rules-5)
      (o/insert ::player {::x 2000 ::y 15})
      o/fire-rules))

(on-window-resize 800 600)

(println (o/query-all @*session ::get-player))

;; example 6

(def rules-6
  (o/ruleset
    {::get-character
     [:what
      [id ::x x]
      [id ::y y]]}))

(reset! *session
  (-> (reduce o/add-rule (o/->session) rules-6)
      (o/insert ::player {::x 20 ::y 15})
      (o/insert ::enemy {::x 5 ::y 5})
      o/fire-rules))

(println (o/query-all @*session ::get-character))

;; example 7

(reset! *session (reduce o/add-rule (o/->session) rules-6))

(swap! *session
  (fn [session]
    (o/fire-rules
      (reduce (fn [session id]
                (o/insert session id {::x (rand-int 50) ::y (rand-int 50)}))
              session
              (range 5)))))

(println (o/query-all @*session ::get-character))

;; example 8

(defn test-derived-fact! [rules]
  (reset! *session (reduce o/add-rule (o/->session) rules))
  (swap! *session
    (fn [session]
      (o/fire-rules
        (reduce (fn [session id]
                  (o/insert session id {::x (rand-int 50) ::y (rand-int 50)}))
                session
                (range 5)))))
  (swap! *session
    (fn [session]
      (-> session
          (o/retract 0 ::x)
          (o/retract 0 ::y)
          o/fire-rules))))

(test-derived-fact!
  (o/ruleset
    {::get-character
     [:what
      [id ::x x]
      [id ::y y]
      :then
      (->> (o/query-all o/*session* ::get-character)
           (o/insert o/*session* ::derived ::all-characters)
           o/reset!)]

     ::print-all-characters
     [:what
      [::derived ::all-characters all-characters]
      :then
      (println "All characters:" all-characters)]}))

(test-derived-fact!
  (o/ruleset
    {::get-character
     [:what
      [id ::x x]
      [id ::y y]
      :then-finally
      (->> (o/query-all o/*session* ::get-character)
           (o/insert o/*session* ::derived ::all-characters)
           o/reset!)]

     ::print-all-characters
     [:what
      [::derived ::all-characters all-characters]
      :then
      (println "All characters:" all-characters)]}))

;; example 9

(def facts (->> (o/query-all @*session)
                (remove (fn [[id]]
                          (= id ::derived)))
                pr-str
                edn/read-string))

(swap! *session
  (fn [session]
    (o/fire-rules
      (reduce o/insert session facts))))

;; example 10

(s/def ::x number?)
(s/def ::y number?)
(s/def ::width (s/and number? pos?))
(s/def ::height (s/and number? pos?))

(reset! *session (reduce o/add-rule (o/->session) rules-6))

(->
  (swap! *session
    (fn [session]
      (-> session
          (o/insert ::player {::x 20 ::y 15 ::width 0 ::height 15})
          o/fire-rules)))
  ;; print spec error but don't stop execution
  #?(:clj (try (catch Exception e (println (.getMessage e))))))
