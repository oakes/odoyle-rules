(ns odoyle.rules-test
  (:require [clojure.test :refer :all]
            [odoyle.rules :as o]))

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
      ((fn [session]
         (is (= 1 (count (o/query-all session ::duplicate-facts))))
         (is (= "red" (:c (o/query session ::duplicate-facts))))
         session))
      (o/insert ::bob ::color "green")
      ((fn [session]
         (is (= 1 (count (o/query-all session ::duplicate-facts))))
         (is (= "green" (:c (o/query session ::duplicate-facts))))
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
      ((fn [session]
         (is (= 1 (count (o/query-all session ::updating-facts))))
         (is (= ::zach (:z (o/query session ::updating-facts))))
         session))
      (o/insert ::yair ::left-of ::xavier)
      ((fn [session]
         (is (= 1 (count (o/query-all session ::updating-facts))))
         (is (= ::xavier (:z (o/query session ::updating-facts))))
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
      ((fn [session]
         (is (= 1 (count (o/query-all session ::updating-facts-diff-nodes))))
         session))
      (o/insert ::yair ::left-of ::xavier)
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
      ((fn [session]
         (is (= ::alice (:a (o/query session ::rule1))))
         (is (= ::zach (:z (o/query session ::rule2))))
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
      ((fn [session]
         (is (= 0 (count (o/query-all session ::complex-cond))))
         session))
      (o/insert ::yair ::left-of ::charlie)
      ((fn [session]
         (is (= 1 (count (o/query-all session ::complex-cond))))
         session))))

