(ns odoyle.rules-test
  (:require [clojure.test :refer [deftest is]]
            [odoyle.rules :as o]
            [clojure.spec.test.alpha :as st]))

(st/instrument)
(st/unstrument 'odoyle.rules/insert)

(deftest num-of-conditions-not=-num-of-facts
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::num-conds-and-facts
           [:what
            [b ::color "blue"]
            [y ::left-of z]
            [a ::color "maize"]
            [y ::right-of b]
            [x ::height h]
            :then
            (is (= a ::alice))
            (is (= b ::bob))
            (is (= y ::yair))
            (is (= z ::zach))]}))
      (o/insert ::bob ::color "blue")
      (o/insert ::yair ::left-of ::zach)
      (o/insert ::alice ::color "maize")
      (o/insert ::yair ::right-of ::bob)
      (o/insert ::xavier ::height 72)
      (o/insert ::thomas ::height 72)
      (o/insert ::george ::height 72)
      o/fire-rules
      ((fn [session]
         (is (= 3 (count (o/query-all session ::num-conds-and-facts))))
         session))))

(deftest adding-facts-out-of-order
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::out-of-order
           [:what
            [x ::right-of y]
            [y ::left-of z]
            [z ::color "red"]
            [a ::color "maize"]
            [b ::color "blue"]
            [c ::color "green"]
            [d ::color "white"]
            [s ::on "table"]
            [y ::right-of b]
            [a ::left-of d]
            :then
            (is (= a ::alice))
            (is (= b ::bob))
            (is (= y ::yair))
            (is (= z ::zach))]}))
      (o/insert ::xavier ::right-of ::yair)
      (o/insert ::yair ::left-of ::zach)
      (o/insert ::zach ::color "red")
      (o/insert ::alice ::color "maize")
      (o/insert ::bob ::color "blue")
      (o/insert ::charlie ::color "green")
      (o/insert ::seth ::on "table")
      (o/insert ::yair ::right-of ::bob)
      (o/insert ::alice ::left-of ::david)
      (o/insert ::david ::color "white")
      o/fire-rules
      ((fn [session]
         (is (= 1 (count (o/query-all session ::out-of-order))))
         session))))

(deftest duplicate-facts
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::duplicate-facts
           [:what
            [x ::self y]
            [x ::color c]
            [y ::color c]]}))
      (o/insert ::bob ::self ::bob)
      (o/insert ::bob ::color "red")
      o/fire-rules
      ((fn [session]
         (is (= 1 (count (o/query-all session ::duplicate-facts))))
         (is (= "red" (:c (first (o/query-all session ::duplicate-facts)))))
         session))
      (o/insert ::bob ::color "green")
      o/fire-rules
      ((fn [session]
         (is (= 1 (count (o/query-all session ::duplicate-facts))))
         (is (= "green" (:c (first (o/query-all session ::duplicate-facts)))))
         session))))

(deftest removing-facts
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::removing-facts
           [:what
            [b ::color "blue"]
            [y ::left-of z]
            [a ::color "maize"]
            [y ::right-of b]]}))
      (o/insert ::bob ::color "blue")
      (o/insert ::yair ::left-of ::zach)
      (o/insert ::alice ::color "maize")
      (o/insert ::yair ::right-of ::bob)
      o/fire-rules
      ((fn [session]
         (is (= 1 (count (o/query-all session ::removing-facts))))
         session))
      (o/retract ::yair ::right-of)
      ((fn [session]
         (is (= 0 (count (o/query-all session ::removing-facts))))
         session))
      (o/retract ::bob ::color)
      (o/insert ::bob ::color "blue")
      (o/insert ::yair ::right-of ::bob)
      o/fire-rules
      ((fn [session]
         (is (= 1 (count (o/query-all session ::removing-facts))))
         session))))

(deftest updating-facts
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::updating-facts
           [:what
            [b ::color "blue"]
            [y ::left-of z]
            [a ::color "maize"]
            [y ::right-of b]]}))
      (o/insert ::bob ::color "blue")
      (o/insert ::yair ::left-of ::zach)
      (o/insert ::alice ::color "maize")
      (o/insert ::yair ::right-of ::bob)
      o/fire-rules
      ((fn [session]
         (is (= 1 (count (o/query-all session ::updating-facts))))
         (is (= ::zach (:z (first (o/query-all session ::updating-facts)))))
         session))
      (o/insert ::yair ::left-of ::xavier)
      o/fire-rules
      ((fn [session]
         (is (= 1 (count (o/query-all session ::updating-facts))))
         (is (= ::xavier (:z (first (o/query-all session ::updating-facts)))))
         session))))

(deftest updating-facts-in-different-alpha-nodes
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::updating-facts-diff-nodes
           [:what
            [b ::color "blue"]
            [y ::left-of ::zach]
            [a ::color "maize"]
            [y ::right-of b]]}))
      (o/insert ::bob ::color "blue")
      (o/insert ::yair ::left-of ::zach)
      (o/insert ::alice ::color "maize")
      (o/insert ::yair ::right-of ::bob)
      o/fire-rules
      ((fn [session]
         (is (= 1 (count (o/query-all session ::updating-facts-diff-nodes))))
         session))
      (o/insert ::yair ::left-of ::xavier)
      o/fire-rules
      ((fn [session]
         (is (= 0 (count (o/query-all session ::updating-facts-diff-nodes))))
         session))))

(deftest facts-can-be-stored-in-different-alpha-nodes
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::rule1
           [:what
            [a ::left-of ::zach]]
           ::rule2
           [:what
            [a ::left-of z]]}))
      (o/insert ::alice ::left-of ::zach)
      o/fire-rules
      ((fn [session]
         (is (= ::alice (:a (first (o/query-all session ::rule1)))))
         (is (= ::zach (:z (first (o/query-all session ::rule2)))))
         session))))

(deftest complex-conditions
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::complex-cond
           [:what
            [b ::color "blue"]
            [y ::left-of z]
            [a ::color "maize"]
            [y ::right-of b]
            :when
            (not= z ::zach)]}))
      (o/insert ::bob ::color "blue")
      (o/insert ::yair ::left-of ::zach)
      (o/insert ::alice ::color "maize")
      (o/insert ::yair ::right-of ::bob)
      o/fire-rules
      ((fn [session]
         (is (= 0 (count (o/query-all session ::complex-cond))))
         session))
      (o/insert ::yair ::left-of ::charlie)
      o/fire-rules
      ((fn [session]
         (is (= 1 (count (o/query-all session ::complex-cond))))
         session))))

(deftest out-of-order-joins-between-id-and-value
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::rule1
           [:what
            [b ::right-of ::alice]
            [y ::right-of b]
            [b ::color "blue"]]}))
      (o/insert ::bob ::right-of ::alice)
      (o/insert ::bob ::color "blue")
      (o/insert ::yair ::right-of ::bob)
      o/fire-rules
      ((fn [session]
         (is (= 1 (count (o/query-all session ::rule1))))
         session))))

(deftest simple-conditions
  (let [*count (atom 0)]
    (-> (reduce o/add-rule (o/->session)
          (o/ruleset
            {::simple-cond
             [:what
              [b ::color "blue"]
              :when
              false
              :then
              (swap! *count inc)]}))
        (o/insert ::bob ::color "blue")
        o/fire-rules
        ((fn [session]
           (is (= 0 @*count))
           session)))))

(deftest queries
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::get-person
           [:what
            [id ::color color]
            [id ::left-of left-of]
            [id ::height height]]}))
      (o/insert ::bob ::color "blue")
      (o/insert ::bob ::left-of ::zach)
      (o/insert ::bob ::height 72)
      (o/insert ::alice ::color "green")
      (o/insert ::alice ::left-of ::bob)
      (o/insert ::alice ::height 64)
      (o/insert ::charlie ::color "red")
      (o/insert ::charlie ::left-of ::alice)
      (o/insert ::charlie ::height 72)
      o/fire-rules
      ((fn [session]
         (is (= 3 (count (o/query-all session ::get-person))))
         session))))

(deftest query-all-facts
  (let [rules (o/ruleset
                {::get-person
                 [:what
                  [id ::color color]
                  [id ::left-of left-of]
                  [id ::height height]]})]
    (-> (reduce o/add-rule (o/->session) rules)
        (o/insert ::bob ::color "blue")
        (o/insert ::bob ::left-of ::zach)
        (o/insert ::bob ::height 72)
        (o/insert ::alice ::color "green")
        (o/insert ::alice ::left-of ::bob)
        (o/insert ::alice ::height 64)
        (o/insert ::charlie ::color "red")
        (o/insert ::charlie ::left-of ::alice)
        (o/insert ::charlie ::height 72)
        ;; insert and retract a fact to make sure
        ;; it isn't returned by query-all
        (o/insert ::zach ::color "blue")
        (o/retract ::zach ::color)
        ((fn [session]
           (let [facts (o/query-all session)
                 ;; make a new session and insert the facts we retrieved
                 new-session (reduce o/add-rule (o/->session) rules)
                 new-session (reduce o/insert new-session facts)]
             (is (= 9 (count facts)))
             (is (= 3 (count (o/query-all new-session ::get-person))))
             new-session))))))

(deftest creating-a-ruleset
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::bob
           [:what
            [b ::color "blue"]
            [b ::right-of a]
            :then
            (is (= a ::alice))
            (is (= b ::bob))]
           ::alice
           [:what
            [a ::color "red"]
            [a ::left-of b]
            :then
            (is (= a ::alice))
            (is (= b ::bob))]}))
      (o/insert ::bob ::color "blue")
      (o/insert ::bob ::right-of ::alice)
      (o/insert ::alice ::color "red")
      (o/insert ::alice ::left-of ::bob)
      o/fire-rules))

(deftest dont-trigger-rule-when-updating-certain-facts
  (let [*count (atom 0)]
    (-> (reduce o/add-rule (o/->session)
          (o/ruleset
            {::dont-trigger
             [:what
              [b ::color "blue"]
              [a ::color c {:then false}]
              :then
              (swap! *count inc)]}))
        (o/insert ::bob ::color "blue")
        o/fire-rules
        (o/insert ::alice ::color "red")
        o/fire-rules
        (o/insert ::alice ::color "maize")
        o/fire-rules
        ((fn [session]
           (is (= 1 @*count))
           session)))))

(deftest inserting-inside-a-rule
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::rule1
           [:what
            [b ::color "blue"]
            [::alice ::color c {:then false}]
            :then
            (o/reset! (o/insert o/*session* ::alice ::color "maize"))]}))
      (o/insert ::bob ::color "blue")
      (o/insert ::alice ::color "red")
      o/fire-rules
      ((fn [session]
         (is (= "maize" (:c (first (o/query-all session ::rule1)))))
         session))))

(deftest inserting-inside-a-rule-can-trigger-more-than-once
  (let [*count (atom 0)]
    (-> (reduce o/add-rule (o/->session)
          (o/ruleset
            {::rule1
             [:what
              [b ::color "blue"]
              :then
              (-> o/*session*
                  (o/insert ::alice ::color "maize")
                  (o/insert ::charlie ::color "gold")
                  o/reset!)]
             ::rule2
             [:what
              [::alice ::color c1]
              [other-person ::color c2]
              :when
              (not= other-person ::alice)
              :then
              (swap! *count inc)]}))
        (o/insert ::alice ::color "red")
        (o/insert ::bob ::color "blue")
        o/fire-rules
        ((fn [session]
           (is (= 3 @*count))
           session)))))

(deftest inserting-inside-a-rule-cascades
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::rule1
           [:what
            [b ::color "blue"]
            :then
            (o/reset! (o/insert o/*session* ::charlie ::right-of ::bob))]
           ::rule2
           [:what
            [c ::right-of b]
            :then
            (o/reset! (o/insert o/*session* b ::left-of c))]
           ::rule3
           [:what
            [b ::left-of c]]}))
      (o/insert ::bob ::color "blue")
      o/fire-rules
      ((fn [session]
         (is (= 1 (count (o/query-all session ::rule1))))
         (is (= 1 (count (o/query-all session ::rule2))))
         (is (= 1 (count (o/query-all session ::rule3))))
         session))))

(deftest conditions-can-use-external-values
  (let [*allow-rule-to-fire (atom false)]
    (-> (reduce o/add-rule (o/->session)
          (o/ruleset
            {::rule1
             [:what
              [a ::left-of b]
              :when
              @*allow-rule-to-fire]}))
        (o/insert ::alice ::left-of ::zach)
        o/fire-rules
        ((fn [session]
           (reset! *allow-rule-to-fire true)
           session))
        (o/insert ::alice ::left-of ::bob)
        o/fire-rules
        ((fn [session]
           (is (= 1 (count (o/query-all session ::rule1))))
           (reset! *allow-rule-to-fire false)
           session))
        (o/insert ::alice ::left-of ::zach)
        o/fire-rules
        ((fn [session]
           (is (= 0 (count (o/query-all session ::rule1))))
           session)))))

(deftest id+attr-combos-can-be-stored-in-multiple-alpha-nodes
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::get-alice
           [:what
            [::alice ::color color]
            [::alice ::height height]]
           ::get-person
           [:what
            [id ::color color]
            [id ::height height]]}))
      (o/insert ::alice ::color "blue")
      (o/insert ::alice ::height 60)
      o/fire-rules
      ((fn [session]
         (let [alice (first (o/query-all session ::get-alice))]
           (is (= "blue" (:color alice)))
           (is (= 60 (:height alice))))
         session))
      (o/retract ::alice ::color)
      ((fn [session]
         (is (= 0 (count (o/query-all session ::get-alice))))
         session))))

(deftest ids-can-be-arbitrary-integers
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::rule1
           [:what
            [b ::color "blue"]
            [y ::left-of z]
            [a ::color "maize"]
            [y ::right-of b]
            [z ::left-of b]
            :then
            (is (= a ::alice))
            (is (= b ::bob))
            (is (= y ::yair))
            (is (= z 1))]}))
      (o/insert ::bob ::color "blue")
      (o/insert ::yair ::left-of 1)
      (o/insert ::alice ::color "maize")
      (o/insert ::yair ::right-of ::bob)
      (o/insert 1 ::left-of ::bob)
      o/fire-rules))

(deftest join-value-with-id
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::rule1
           [:what
            [b ::left-of id]
            [id ::color color]
            [id ::height height]]}))
      (o/insert ::alice ::color "blue")
      (o/insert ::alice ::height 60)
      (o/insert ::bob ::left-of ::alice)
      (o/insert ::charlie ::color "green")
      (o/insert ::charlie ::height 72)
      (o/insert ::bob ::left-of ::charlie)
      o/fire-rules
      ((fn [session]
         (is (= 1 (count (o/query-all session ::rule1))))
         session))))

(deftest multiple-joins
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::rule1
           [:what
            [pid ::kind :player]
            [pid ::color pcolor]
            [pid ::height pheight]
            [eid ::kind kind]
            [eid ::color ecolor {:then false}]
            [eid ::height eheight {:then false}]
            :when
            (not= kind :player)
            :then
            (-> o/*session*
                (o/insert eid ::color "green")
                (o/insert eid ::height 70)
                o/reset!)]}))
      (o/insert 1 {::kind :player
                   ::color "red"
                   ::height 72})
      (o/insert 2 {::kind :enemy
                   ::color "blue"
                   ::height 60})
      o/fire-rules
      ((fn [session]
         (is (= "green" (:ecolor (first (o/query-all session ::rule1)))))
         session))))

(deftest join-followed-by-non-join
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::rule1
           [:what
            [id ::x x]
            [id ::y y]
            [id ::xv xv]
            [id ::yv yv]
            [::bob ::left-of z]]}))
      (o/insert ::bob ::left-of ::zach)
      (o/insert ::alice {::x 0 ::y 0 ::xv 1 ::yv 1})
      (o/insert ::charlie {::x 1 ::y 1 ::xv 0 ::yv 0})
      o/fire-rules
      ((fn [session]
         (is (= 2 (count (o/query-all session ::rule1))))
         session))))

(deftest only-last-condition-can-fire
  (let [*count (atom 0)]
    (-> (reduce o/add-rule (o/->session)
          (o/ruleset
            {::rule1
             [:what
              [id ::left-of ::bob {:then false}]
              [id ::color color {:then false}]
              [::alice ::height height]
              :then
              (swap! *count inc)]}))
        (o/insert ::alice ::height 60) ;; out of order
        (o/insert ::alice ::left-of ::bob)
        (o/insert ::alice ::color "blue")
        o/fire-rules
        ((fn [session]
           (is (= 1 @*count))
           session))
        (o/retract ::alice ::height)
        (o/retract ::alice ::left-of)
        (o/retract ::alice ::color)
        (o/insert ::alice ::height 60)
        (o/insert ::alice ::left-of ::bob)
        (o/insert ::alice ::color "blue")
        o/fire-rules
        ((fn [session]
           (is (= 2 @*count))
           session))
        (o/insert ::alice ::left-of ::bob)
        (o/insert ::alice ::color "blue")
        o/fire-rules
        ((fn [session]
           (is (= 2 @*count))
           session))
        (o/insert ::alice ::height 60)
        o/fire-rules
        ((fn [session]
           (is (= 3 @*count))
           session)))))

(deftest avoid-unnecessary-rule-firings
  (let [*count (atom 0)]
    (-> (reduce o/add-rule (o/->session)
          (o/ruleset
            {::get-person
             [:what
              [id ::color color]
              [id ::left-of left-of]
              [id ::height height]
              :then
              (swap! *count inc)]}))
        (o/insert ::bob ::color "blue")
        (o/insert ::bob ::left-of ::zach)
        (o/insert ::bob ::height 72)
        (o/insert ::alice ::color "blue")
        (o/insert ::alice ::left-of ::zach)
        (o/insert ::alice ::height 72)
        o/fire-rules
        (o/insert ::alice ::color "blue")
        o/fire-rules
        ((fn [session]
           (is (= 3 @*count))
           session)))))

(deftest then-finally
  (let [*trigger-count (atom 0)
        *all-people (atom [])]
    (-> (reduce o/add-rule (o/->session)
          (o/ruleset
            {::get-person
             [:what
              [id ::color color]
              [id ::left-of left-of]
              [id ::height height]
              :then-finally
              (->> (o/query-all o/*session* ::get-person)
                   (o/insert o/*session* ::people ::all)
                   o/reset!)]
             ::all-people
             [:what
              [::people ::all all-people]
              :then
              (reset! *all-people all-people)
              (swap! *trigger-count inc)]}))
        (o/insert ::bob ::color "blue")
        (o/insert ::bob ::left-of ::zach)
        (o/insert ::bob ::height 72)
        (o/insert ::alice ::color "blue")
        (o/insert ::alice ::left-of ::zach)
        (o/insert ::alice ::height 72)
        o/fire-rules
        ((fn [session]
           (is (= 2 (count @*all-people)))
           (is (= 1 @*trigger-count))
           session))
        (o/retract ::alice ::color)
        o/fire-rules
        ((fn [session]
           (is (= 1 (count @*all-people)))
           (is (= 2 @*trigger-count))
           session)))))

;; based on https://github.com/raquo/Airstream#frp-glitches
(deftest frp-glitch
  (let [*output (atom [])]
    (-> (reduce o/add-rule (o/->session)
          (o/ruleset
            {::is-positive
             [:what
              [::number ::any any-num]
              :then
              (o/insert! ::number ::positive? (pos? any-num))]
             
             ::doubled-numbers
             [:what
              [::number ::any any-num]
              :then
              (o/insert! ::number ::doubled (* 2 any-num))]
             
             ::combined
             [:what
              [::number ::positive? positive?]
              [::number ::doubled doubled]
              :then
              (o/insert! ::number ::combined [doubled positive?])]
             
             ::print-combined
             [:what
              [::number ::combined combined]
              :then
              (swap! *output conj combined)]}))
        (o/insert ::number ::any -1)
        o/fire-rules
        (o/insert ::number ::any 1)
        o/fire-rules
        ((fn [session]
           (is (= @*output [[-2 false] [2 true]]))
           session)))))

(deftest recursion
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::get-person
           [:what
            [id ::color color]
            [id ::left-of left-of]
            [id ::height height]
            [id ::friends friends {:then not=}]
            :then-finally
            (->> (o/query-all o/*session* ::get-person)
                 (reduce #(assoc %1 (:id %2) %2) {})
                 (o/insert! ::people ::by-id))]

           ::update-friends
           [:what
            [id ::friend-ids friend-ids]
            [::people ::by-id id->person]
            :then
            (->> (mapv id->person friend-ids)
                 (o/insert! id ::friends))]}))
      (o/insert ::bob ::color "blue")
      (o/insert ::bob ::left-of ::zach)
      (o/insert ::bob ::height 72)
      (o/insert ::bob ::friend-ids [::alice ::charlie])
      (o/insert ::alice ::color "blue")
      (o/insert ::alice ::left-of ::zach)
      (o/insert ::alice ::height 72)
      (o/insert ::alice ::friend-ids [])
      (o/insert ::charlie ::color "red")
      (o/insert ::charlie ::left-of ::bob)
      (o/insert ::charlie ::height 70)
      (o/insert ::charlie ::friend-ids [::alice])
      (o/insert ::people ::by-id {})
      o/fire-rules
      ((fn [session]
         (let [people (o/query-all session ::get-person)
               bob (first (filter #(= ::bob (:id %)) people))
               alice (first (filter #(= ::alice (:id %)) people))
               charlie (first (filter #(= ::charlie (:id %)) people))]
           (is (= 3 (count people)))
           (is (= [alice charlie] (:friends bob)))
           (is (= [] (mapv :id (:friends alice))))
           (is (= [alice] (:friends charlie))))
         session))))

;; normally, the {:then not=} would be enough to prevent an
;; infinite loop here. but because the other facts are joined
;; with the first one via `id`, they too will be updated when
;; the ::left-of fact is updated. therefore, they must have
;; {:then false} to prevent the infinite loop from happening.
(deftest avoid-infinite-loop-when-updating-fact-whose-value-is-joined
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::rule1
           [:what
            [b ::left-of id {:then not=}]
            [id ::color color {:then false}]
            [id ::height height {:then false}]
            :then
            (o/insert! b ::left-of ::charlie)]}))
      (o/insert ::bob ::left-of ::alice)
      (o/insert ::alice ::color "blue")
      (o/insert ::alice ::height 60)
      (o/insert ::charlie ::color "green")
      (o/insert ::charlie ::height 72)
      o/fire-rules
      ((fn [session]
         (is (= ::charlie (-> (o/query-all session ::rule1)
                              first
                              :id)))
         session))))

(deftest recursion-limit
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::rule1
           [:what
            [::alice ::color c]
            :then
            (o/reset! (o/insert o/*session* ::alice ::height 15))]
           
           ::rule2
           [:what
            [::alice ::height height]
            :then
            (o/reset! (o/insert o/*session* ::alice ::age 10))]
           
           ::rule3
           [:what
            [::alice ::age age]
            :then
            (o/reset! (-> o/*session*
                          (o/insert ::alice ::color "maize")
                          (o/insert ::bob ::age 10)))]
           
           ::rule4
           [:what
            [::bob ::age age]
            :then
            (o/reset! (o/insert o/*session* ::bob ::height 15))]
           
           ::rule5
           [:what
            [::bob ::height height]
            :then
            (o/reset! (o/insert o/*session* ::bob ::age 10))]
           
           ::rule6
           [:what
            [::bob ::color c]
            :then
            (o/reset! (o/insert o/*session* ::bob ::color c))]}))
      (o/insert ::alice ::color "red")
      (o/insert ::bob ::color "blue")
      ((fn [session]
        (is (thrown? #?(:clj Exception :cljs js/Error)
                     (o/fire-rules session)))))))

(deftest non-deterministic-behavior
  (let [*count (atom 0)]
    (-> (reduce o/add-rule (o/->session)
          (o/ruleset
            {::rule1
             [:what
              [id ::color "blue"]
              :then
              (swap! *count inc)
              (o/insert! id ::color "green")]
             
             ::rule2
             [:what
              [id ::color "blue"]
              :then
              (swap! *count inc)]
             
             ::rule3
             [:what
              [id ::color "blue"]
              :then
              (swap! *count inc)]}))
        (o/insert ::alice ::color "blue")
        o/fire-rules
        ((fn [session]
           (is (= 3 @*count))
           session)))))

(deftest dynamic-rule
  (let [*then-count (atom 0)
        *then-finally-count (atom 0)]
    (-> (o/add-rule
          (o/->session)
          (o/->rule
            ::player
            [:what
             ['id :player/x 'x {:then not=}]
             ['id :player/y 'y {:then not=}]
             :when
             (fn [{:keys [x y] :as match}]
               (and (pos? x) (pos? y)))
             :then
             (fn [{:keys [id] :as match}]
               (swap! *then-count inc))
             :then-finally
             (fn []
               (swap! *then-finally-count inc))]))
        (o/insert 1 {:player/x 3 :player/y 1})
        (o/insert 2 {:player/x 5 :player/y 2})
        (o/insert 3 {:player/x 7 :player/y -1})
        o/fire-rules
        (o/insert 1 {:player/x 3 :player/y 1})
        o/fire-rules
        ((fn [session]
           (is (= 2 (count (o/query-all session ::player))))
           (is (= 2 @*then-count))
           (is (= 1 @*then-finally-count))
           session)))))

;; this is a demonstration of how literal values can cause a rule to fire
;; more often than when a binding is used. the technical reason is that
;; literal values are checked earlier on (in the alpha network).
;; when the value changes and then returns to its original value,
;; the entire match is retracted and then re-inserted, causing the rule to fire
;; despite the {:then false} usage.
;; with bindings, this is usually an in-place update, and the match is never
;; fully retracted, so it doesn't need to fire the rule again.
;; this difference in behavior isn't ideal but for now i can't think of a nice fix...
(deftest literal-values-with-then-option-can-cause-extra-rule-firings
  (let [*count-1 (atom 0)
        *count-2 (atom 0)
        ruleset-1 (o/ruleset
                    {::rule1
                     [:what
                      [id ::retired retired {:then false}] ;; value is a binding
                      [id ::age age]
                      :when
                      (not retired)
                      :then
                      (swap! *count-1 inc)]
                     
                     ::rule2
                     [:what
                      [id ::age age]
                      :then
                      (o/insert! id ::retired true)]
                     
                     ::rule3
                     [:what
                      [id ::retired true]
                      :then
                      (o/insert! id ::retired false)]})
        ruleset-2 (o/ruleset
                    {::rule1
                     [:what
                      [id ::retired false {:then false}] ;; value is a literal
                      [id ::age age]
                      :then
                      (swap! *count-2 inc)]
                     
                     ::rule2
                     [:what
                      [id ::age age]
                      :then
                      (o/insert! id ::retired true)]
                     
                     ::rule3
                     [:what
                      [id ::retired true]
                      :then
                      (o/insert! id ::retired false)]})
        session-1 (reduce o/add-rule (o/->session) ruleset-1)
        session-2 (reduce o/add-rule (o/->session) ruleset-2)]
    (-> session-1
        (o/insert ::bob {::retired false ::age 50})
        o/fire-rules
        ((fn [session]
           (is (= 1 @*count-1))
           session)))
    (-> session-2
        (o/insert ::bob {::retired false ::age 50})
        o/fire-rules
        ((fn [session]
           (is (= 2 @*count-2))
           session)))))

(deftest contains
  (-> (reduce o/add-rule (o/->session)
        (o/ruleset
          {::num-conds-and-facts
           [:what
            [b ::color "blue"]]}))
      (o/insert ::bob ::color "blue")
      ((fn [session]
         (is (o/contains? session ::bob ::color))
         session))
      (o/retract ::bob ::color)
      ((fn [session]
         (is (not (o/contains? session ::bob ::color)))
         session))))

