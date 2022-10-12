(ns odoyle.readme
  (:require [odoyle.rules :as o]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [#?(:clj clojure.edn :cljs cljs.reader) :as edn]))

(s/def ::total number?)
(s/def ::delta number?)
(s/def ::x number?)
(s/def ::y number?)
(s/def ::width (s/and number? pos?))
(s/def ::height (s/and number? pos?))
(s/def ::character (s/keys :req-un [::x ::y]))
(s/def ::all-characters (s/coll-of ::character))
(s/def ::characters-within-window ::all-characters)
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
      (println "Query from inside rule:" (o/query-all session ::player))]}))

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
      (->> (o/query-all session ::character)
           (o/insert session ::derived ::all-characters)
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
      (->> (o/query-all session ::character)
           (o/insert session ::derived ::all-characters)
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
            (first (o/query-all session ::window))] ;; warning: this will not be reactive!
        (->> (o/query-all session ::character)
             (filterv #(within? % window-width window-height))
             (o/insert session ::derived ::characters-within-window)
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
      (->> (o/query-all session ::character)
           (o/insert session ::derived ::all-characters)
           o/reset!)]

     ::characters-within-window
     [:what
      [::window ::width window-width]
      [::window ::height window-height]
      [::derived ::all-characters all-characters]
      :then
      (->> all-characters
           (filterv #(within? % window-width window-height))
           (o/insert session ::derived ::characters-within-window)
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

(reset! *session
  (->> rules-4
       (map (fn [rule]
              (o/wrap-rule rule
                           {:when
                            (fn [f session match]
                              (println :when (:name rule) match)
                              (f session match))
                            :then
                            (fn [f session match]
                              (println :then (:name rule) match)
                              (f session match))
                            :then-finally
                            (fn [f session]
                              (println :then-finally (:name rule))
                              (f session))})))
       (reduce o/add-rule (o/->session))))

(swap! *session
  (fn [session]
    (-> session
        (o/insert ::player {::x 20 ::y 15})
        (o/insert ::time {::total 100 ::delta 0.1})
        o/fire-rules)))

;; example 11

(reset! *session (reduce o/add-rule (o/->session) rules-6))

(->
  (swap! *session
    (fn [session]
      (-> session
          (o/insert ::player {::x 20 ::y 15 ::width 0 ::height 15})
          o/fire-rules)))
  ;; print spec error but don't stop execution
  #?(:clj (try (catch Exception e (println (.getMessage e))))
     :cljs (try (catch js/Error e (println (.-message e))))))

;; example 12

(def rules-12
  (o/ruleset
    {::character
     [:what
      [id ::x x]
      [id ::y y]
      :then
      (o/insert! id ::character match)]

      ::move-character
      [:what
       [::time ::delta dt]
       [id ::character ch {:then false}]
       :then
       (o/insert! id {::x (+ (:x ch) dt) ::y (+ (:y ch) dt)})]}))

(reset! *session
  (-> (reduce o/add-rule (o/->session) rules-12)
      (o/insert ::player {::x 20 ::y 15})
      (o/insert ::enemy {::x 5 ::y 5})
      (o/insert ::time ::delta 0.1)
      o/fire-rules))

(println (o/query-all @*session ::character))

;; example 13

(def rule
  (o/->rule
    ::character
    {:what
     '[[id ::x x]
       [id ::y y]]
     :when
     (fn [session {:keys [x y] :as match}]
       (and (pos? x) (pos? y)))
     :then
     (fn [session match]
       (println "This will fire twice"))
     :then-finally
     (fn [session]
       (println "This will fire once"))}))

(-> (o/add-rule (o/->session) rule)
    (o/insert 1 {::x 3 ::y 1})
    (o/insert 2 {::x 5 ::y 2})
    (o/insert 3 {::x 7 ::y -1})
    o/fire-rules
    (o/query-all ::character)
    println)

(defn ->character-rule [id]
  (o/->rule id
    {:what
     [[id ::x 'x]
      [id ::y 'y]]}))

(reset! *session
  (-> (o/->session)
      (o/add-rule (->character-rule ::player))
      (o/add-rule (->character-rule ::enemy))
      (o/insert ::player {::x 20 ::y 15})
      (o/insert ::enemy {::x 5 ::y 5})
      o/fire-rules))

(println (first (o/query-all @*session ::player)))
(println (first (o/query-all @*session ::enemy)))
