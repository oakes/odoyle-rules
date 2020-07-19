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
      (o/insert ::george ::height 72)))
