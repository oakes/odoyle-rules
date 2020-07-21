(require '[odoyle.rules :as o])

(def orig-session
  (reduce o/add-rule (o/->session)
        (o/ruleset
          {::num-conds-and-facts
           [:what
            [b ::color "blue"]
            [y ::left-of z]
            [a ::color "maize"]
            [y ::right-of b]
            [x ::height h]
            :then
            (= a ::alice)
            (= b ::bob)
            (= y ::yair)
            (= z ::zach)]})))

(time
  (loop [counter 0
         session orig-session]
    (if (= counter 10000)
      session
      (recur
        (inc counter)
        (-> session
            (o/insert ::bob ::color "blue")
            (o/insert ::yair ::left-of ::zach)
            (o/insert ::alice ::color "maize")
            (o/insert ::yair ::right-of ::bob)
            (o/insert ::xavier ::height 72)
            (o/insert ::thomas ::height 72)
            (o/insert ::george ::height 72))))))
