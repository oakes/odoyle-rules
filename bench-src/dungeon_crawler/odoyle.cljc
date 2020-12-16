(ns dungeon-crawler.odoyle
  (:require [dungeon-crawler.move :as move]
            [dungeon-crawler.entities :as e]
            [odoyle.rules :as o #?(:clj :refer :cljs :refer-macros) [ruleset]]))

(def rules
  (o/ruleset
    {::get-entity
     [:what
      [id ::e/kind kind]
      [id ::e/x x]
      [id ::e/y y]
      [id ::e/x-change x-change]
      [id ::e/y-change y-change]
      [id ::e/x-velocity x-velocity]
      [id ::e/y-velocity y-velocity]
      [id ::e/width width]
      [id ::e/height height]]
     
     ::move-enemy
     [:what
      [::time ::total total-time]
      [pid ::e/kind :player]
      [pid ::e/health player-health]
      [pid ::e/x player-x]
      [pid ::e/y player-y]
      [pid ::e/x-velocity player-x-velocity]
      [pid ::e/y-velocity player-y-velocity]
      [eid ::e/kind enemy-kind]
      [eid ::e/health enemy-health]
      [eid ::e/x enemy-x {:then false}]
      [eid ::e/y enemy-y {:then false}]
      [eid ::e/x-velocity enemy-x-velocity {:then false}]
      [eid ::e/y-velocity enemy-y-velocity {:then false}]
      [eid ::distance-from-player distance-from-player {:then false}]
      :when
      (not= enemy-kind :player)
      (> enemy-health 0)
      :then
      (let [enemy {:x enemy-x :y enemy-y :x-velocity enemy-x-velocity :y-velocity enemy-y-velocity}
            player {:x player-x :y player-y :x-velocity player-x-velocity :y-velocity player-y-velocity}
            [xv yv] (move/get-enemy-velocity enemy player player-health distance-from-player)]
        (->> (move/move enemy-x enemy-y xv yv 0.1 true)
             (o/insert o/*session* eid)
             o/reset!))]

     ::update-distance-from-player
     [:what
      [pid ::e/kind :player]
      [pid ::e/x player-x]
      [pid ::e/y player-y]
      [eid ::e/x enemy-x]
      [eid ::e/y enemy-y]
      :when
      (not= eid pid)
      :then
      (-> o/*session*
          (o/insert eid ::distance-from-player (move/calc-distance player-x player-y enemy-x enemy-y))
          o/reset!)]}))

(defn init [session]
  (let [*id (atom 0)]
    (reduce
      (fn [session {:keys [path instances] :as spawn-data}]
        (reduce
          (fn [session instance]
            (let [id (swap! *id inc)]
              (-> session
                  (o/insert id (e/->entity spawn-data instance))
                  (o/insert id ::distance-from-player (inc move/max-aggro-distance)))))
          session
          instances))
      session
      e/spawn-data)))

(def initial-session
  (init (reduce o/add-rule (o/->session) rules)))

(defn tick [session counter]
  (-> session
      (o/insert ::time ::total counter)
      o/fire-rules))

(defn run [iterations]
  (loop [session initial-session
         counter 0]
    (if (= counter iterations)
      session
      (recur (tick session counter) (inc counter)))))

