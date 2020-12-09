(ns benchmark.simple
  (:require [odoyle.rules :as o]))

(def initial-session
  (reduce o/add-rule (o/->session)
        (o/ruleset
          {::num-conds-and-facts
           [:what
            [b ::color "blue"]
            [y ::left-of z]
            [a ::color "maize"]
            [y ::right-of b]
            [x ::height h]]})))

(defn tick [session]
  (-> session
      (o/insert ::bob ::color "blue")
      (o/insert ::yair ::left-of ::zach)
      (o/insert ::alice ::color "maize")
      (o/insert ::yair ::right-of ::bob)
      (o/insert ::xavier ::height 72)
      (o/insert ::thomas ::height 72)
      (o/insert ::george ::height 72)
      o/fire-rules))

(defn bench [iterations]
  (time
    (loop [session initial-session
           counter 0]
      (if (= counter iterations)
        session
        (recur (tick session) (inc counter))))))
