(ns odoyle.readme
  (:require [odoyle.rules :as o]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [#?(:clj clojure.edn :cljs cljs.reader) :as edn]))

(st/instrument)
(st/unstrument 'odoyle.rules/insert)

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
      (o/insert! ::player ::x tt)]}))

(reset! *session
  (-> (reduce o/add-rule (o/->session) rules-2)
      (o/insert ::player ::x 20)
      (o/insert ::player ::y 15)
      o/fire-rules))

;; example 3

(def rules-3
  (o/ruleset
    {::player
     [:what
      [::player ::x x]
      [::player ::y y]]
     ::print-player-when-time-updates
     [:what
      [::time ::total tt]
      :then
      (println "Query from inside rule:" (o/query-all o/*session* ::player))]}))

(reset! *session
  (-> (reduce o/add-rule (o/->session) rules-3)
      (o/insert ::player {::x 20 ::y 15})
      (o/insert ::time ::total 100)
      o/fire-rules))

(println (o/query-all @*session ::player))

;; example 4

(def rules-4
  (o/ruleset
    {::player
     [:what
      [::player ::x x]
      [::player ::y y]]
     ::move-player
     [:what
      [::time ::delta dt]
      [::player ::x x {:then false}]
      :then
      (o/insert! ::player ::x (+ x dt))]}))

(reset! *session
  (-> (reduce o/add-rule (o/->session) rules-4)
      (o/insert ::player {::x 20 ::y 15})
      (o/insert ::time {::total 100 ::delta 0.1})
      o/fire-rules))

(println (o/query-all @*session ::player))

;; example 5

(defn on-window-resize [width height]
  (swap! *session
         (fn [session]
           (-> session
               (o/insert ::window {::width width ::height height})
               o/fire-rules))))

(def rules-5
  (o/ruleset
    {::player
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

(reset! *session
  (-> (reduce o/add-rule (o/->session) rules-5)
      (o/insert ::player {::x 2000 ::y 15})
      o/fire-rules))

(on-window-resize 800 600)

(println (o/query-all @*session ::player))

;; example 6

(def rules-6
  (o/ruleset
    {::character
     [:what
      [id ::x x]
      [id ::y y]]}))

(reset! *session
  (-> (reduce o/add-rule (o/->session) rules-6)
      (o/insert ::player {::x 20 ::y 15})
      (o/insert ::enemy {::x 5 ::y 5})
      o/fire-rules))

(println (o/query-all @*session ::character))

;; example 7

(reset! *session (reduce o/add-rule (o/->session) rules-6))

(swap! *session
  (fn [session]
    (o/fire-rules
      (reduce (fn [session id]
                (o/insert session id {::x (rand-int 50) ::y (rand-int 50)}))
              session
              (range 5)))))

(println (o/query-all @*session ::character))

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
    {::character
     [:what
      [id ::x x]
      [id ::y y]
      :then
      (->> (o/query-all o/*session* ::character)
           (o/insert o/*session* ::derived ::all-characters)
           o/reset!)]

     ::print-all-characters
     [:what
      [::derived ::all-characters all-characters]
      :then
      (println "All characters (using :then):" all-characters)]}))

(test-derived-fact!
  (o/ruleset
    {::character
     [:what
      [id ::x x]
      [id ::y y]
      :then-finally
      (->> (o/query-all o/*session* ::character)
           (o/insert o/*session* ::derived ::all-characters)
           o/reset!)]

     ::print-all-characters
     [:what
      [::derived ::all-characters all-characters]
      :then
      (println "All characters (using :then-finally):" all-characters)]}))

(defn test-derived-fact-with-window-dimensions! [rules]
  (reset! *session (reduce o/add-rule (o/->session) rules))
  (swap! *session
    (fn [session]
      (o/insert session ::window {::width 100 ::height 100})))
  (swap! *session
    (fn [session]
      (o/fire-rules
        (reduce (fn [session [id x y]]
                  (o/insert session id {::x x ::y y}))
                session
                [[0 -10 10]
                 [1 10 10]
                 [2 500 25]
                 [3 25 70]
                 [4 25 -10]
                 [5 20 500]]))))
  (swap! *session
    (fn [session]
      (-> session
          (o/insert ::window {::width 1000 ::height 1000})
          o/fire-rules))))

(defn within? [{:keys [x y]} window-width window-height]
  (and (>= x 0)
       (< x window-width)
       (>= y 0)
       (< y window-height)))

(test-derived-fact-with-window-dimensions!
  (o/ruleset
    {::window
     [:what
      [::window ::width window-width]
      [::window ::height window-height]]

     ::character
     [:what
      [id ::x x]
      [id ::y y]
      :then-finally
      (let [{:keys [window-width window-height]}
            (first (o/query-all o/*session* ::window))] ;; warning: this will not be reactive!
        (->> (o/query-all o/*session* ::character)
             (filterv #(within? % window-width window-height))
             (o/insert o/*session* ::derived ::characters-within-window)
             o/reset!))]

     ::print-characters-within-window
     [:what
      [::derived ::characters-within-window all-characters]
      :then
      (println "Characters within window (not reactive):" all-characters)]}))

(test-derived-fact-with-window-dimensions!
  (o/ruleset
    {::character
     [:what
      [id ::x x]
      [id ::y y]
      :then-finally
      (->> (o/query-all o/*session* ::character)
           (o/insert o/*session* ::derived ::all-characters)
           o/reset!)]

     ::characters-within-window
     [:what
      [::window ::width window-width]
      [::window ::height window-height]
      [::derived ::all-characters all-characters]
      :then
      (->> all-characters
           (filterv #(within? % window-width window-height))
           (o/insert o/*session* ::derived ::characters-within-window)
           o/reset!)]

     ::print-characters-within-window
     [:what
      [::derived ::characters-within-window all-characters]
      :then
      (println "Characters within window (reactive):" all-characters)]}))

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

;; example 11

(def rules-11
  (o/ruleset
    {::character
     [:what
      [id ::x x]
      [id ::y y]
      :then
      (o/insert! id ::character o/*match*)]

      ::move-character
      [:what
       [::time ::delta dt]
       [id ::character ch {:then false}]
       :then
       (o/insert! id {::x (+ (:x ch) dt) ::y (+ (:y ch) dt)})]}))

(reset! *session
  (-> (reduce o/add-rule (o/->session) rules-11)
      (o/insert ::player {::x 20 ::y 15})
      (o/insert ::enemy {::x 5 ::y 5})
      (o/insert ::time ::delta 0.1)
      o/fire-rules))

(println (o/query-all @*session ::character))
