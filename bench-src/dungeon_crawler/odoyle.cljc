(ns dungeon-crawler.odoyle
  (:require [dungeon-crawler.move :as move]
            [dungeon-crawler.entities :as e]
            [odoyle.rules :as o]))

(def rules
  (o/ruleset
    {::entity
     [:what
      [id ::e/kind kind]
      [id ::e/x x]
      [id ::e/y y]
      [id ::e/x-change x-change]
      [id ::e/y-change y-change]
      [id ::e/x-velocity x-velocity]
      [id ::e/y-velocity y-velocity]
      [id ::e/health health]
      :then
      ;; create a derived "all" fact that contains
      ;; all the fields above in a single map
      (-> o/*session*
          ;; give the player's "all" fact a unique id
          ;; so it can be easily distinguished when
          ;; pulled into other rules
          (o/insert (if (= kind :player) ::player id)
                    ::all
                    o/*match*)
          o/reset!)]
     
     ::move-enemy
     [:what
      [::time ::total total-time]
      [::player ::all player {:then false}]
      [eid ::all enemy {:then false}]
      [eid ::distance-from-player distance-from-player {:then false}]
      :when
      (not= eid ::player)
      (> (:health enemy) 0)
      :then
      (let [[xv yv] (move/get-enemy-velocity enemy player distance-from-player)]
        (->> (move/move (:x enemy) (:y enemy) xv yv 0.1 true)
             (o/insert o/*session* eid)
             o/reset!))]

     ::update-distance-from-player
     [:what
      [::player ::all player]
      [eid ::all enemy]
      :when
      (not= eid ::player)
      :then
      (-> o/*session*
          (o/insert eid ::distance-from-player (move/calc-distance (:x player) (:y player) (:x enemy) (:y enemy)))
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

