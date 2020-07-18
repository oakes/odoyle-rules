(ns examples.odoyle
  (:require [odoyle.rules :as o]))

(def rules
  (o/ruleset
    {::remove-dead-enemies
     [:what
      [::enemy ::health health]
      :when
      (= health 0)
      :then
      (o/retract! ::enemy)]

     ::get-player
     [:what
      [::player ::x x]
      [::player ::y y]
      [::player ::health health]]}))

(println rules)

#_
(def *session
  (-> (o/->session rules)
      (o/insert ::player {::x 1 ::y 1 ::health 10})
      (o/insert ::enemy {::x 1 ::y 1 ::health 10})
      (o/insert ::enemy {::x 1 ::y 1 ::health 10})
      (o/insert ::enemy {::x 2 ::y 2 ::health 10})
      (o/insert ::enemy {::x 2 ::y 2 ::health 10})
      atom))

#_
(println (o/query @*session ::get-player))
