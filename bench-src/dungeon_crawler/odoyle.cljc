(ns dungeon-crawler.odoyle
  (:require [dungeon-crawler.move :as move]
            [dungeon-crawler.entities :as e]
            [odoyle.rules :as o #?(:clj :refer :cljs :refer-macros) [ruleset]]))

(def rules
  (o/ruleset
    {::entity
     [:what
      [id ::e/kind kind]
      [id ::e/health health]
      [id ::e/x x]
      [id ::e/y y]
      [id ::e/x-velocity x-velocity]
      [id ::e/y-velocity y-velocity]]
     
     ::move-enemy
     [:extends ::entity
      :what
      [id ::e/kind kind]
      [id ::e/health health]
      [id ::e/x x {:then false}]
      [id ::e/y y {:then false}]
      [id ::e/x-velocity x-velocity {:then false}]
      [id ::e/y-velocity y-velocity {:then false}]
      [id ::distance-from-player distance-from-player {:then false}]
      [pid ::e/kind :player]
      [pid ::e/health player-health]
      [pid ::e/x player-x]
      [pid ::e/y player-y]
      [pid ::e/x-velocity player-x-velocity]
      [pid ::e/y-velocity player-y-velocity]
      [::time ::total total-time]
      :when
      (not= id pid)
      (> health 0)
      :then
      (let [enemy {:x x :y y :x-velocity x-velocity :y-velocity y-velocity}
            player {:x player-x :y player-y :x-velocity player-x-velocity :y-velocity player-y-velocity}
            [xv yv] (move/get-enemy-velocity enemy player player-health distance-from-player)]
        (->> (move/move x y xv yv 0.1 true)
             (o/insert o/*session* id)
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

