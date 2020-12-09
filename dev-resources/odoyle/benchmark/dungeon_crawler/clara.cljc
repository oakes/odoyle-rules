(ns odoyle.benchmark.dungeon-crawler.clara
  (:require [odoyle.benchmark.dungeon-crawler.move :as move]
            [odoyle.benchmark.dungeon-crawler.entities :as e]
            [clara.rules :as clara]
            [clara.rules.accumulators :as acc]
            #?(:clj  [clarax.macros-java :refer [->session]]
               :cljs [clarax.macros-js :refer-macros [->session]])))

(defrecord Entity [id
                   kind
                   x
                   y
                   x-change
                   y-change
                   x-velocity
                   y-velocity
                   game-anchor])
(defrecord Size [id width height])
(defrecord Direction [id value]) ;; :n, :s, ...
(defrecord DistanceFromPlayer [id value]) ;; number - how far is this entity from the player
(defrecord Health [id value]) ;; number
(defrecord Damage [id value]) ;; number - how much damage this entity deals per attack
(defrecord Game [total-time])

(defn init [session]
  (let [*id (atom 0)]
    (clara/fire-rules
      (reduce
        (fn [session {:keys [path instances] :as spawn-data}]
          (reduce
            (fn [session instance]
              (let [id (swap! *id inc)
                    e (reduce-kv
                        (fn [m k v]
                          ;; make keys non-qualified
                          (assoc m (keyword (name k)) v))
                        {:id id}
                        (e/->entity spawn-data instance))]
                (clara/insert session
                  (map->Entity (select-keys e [:id
                                               :kind
                                               :x
                                               :y
                                               :x-change
                                               :y-change
                                               :x-velocity
                                               :y-velocity]))
                  (->Size (:id e) (:width e) (:height e))
                  (->Direction (:id e) (:direction e))
                  (->DistanceFromPlayer (:id e) (inc move/max-attack-distance))
                  (->Health (:id e) (:health e))
                  (->Damage (:id e) (:damage e)))))
            session
            instances))
        (clara/insert session
          (->Game 0))
        e/spawn-data))))

(def initial-session
  (-> {:get-game
       (fn []
         (let [game Game]
           game))

       :get-entities
       (fn []
         (let [entity Entity
               :accumulator (acc/all)]
           entity))

       :move-enemy
       (let [game Game
             player Entity
             :when (= (:kind player) :player)
             player-health Health
             :when (= (:id player) (:id player-health))
             enemy Entity
             :when (and (not= (:game-anchor enemy) game)
                        (not= (:kind enemy) :player))
             enemy-health Health
             :when (and (= (:id enemy) (:id enemy-health))
                        (> (:value enemy-health) 0))
             distance-from-player DistanceFromPlayer
             :when (= (:id enemy) (:id distance-from-player))]
         (let [[xv yv] (move/get-enemy-velocity enemy player (:value player-health) (:value distance-from-player))]
           (clara/retract! enemy)
           (->> (move/move (:x enemy) (:y enemy) xv yv 0.1)
                (merge enemy {:game-anchor game})
                clara/insert-unconditional!)))

       :update-distance-from-player
       (let [player Entity
             :when (= (:kind player) :player)
             enemy Entity
             :when (not= (:id player) (:id enemy))
             distance DistanceFromPlayer
             :when (= (:id distance) (:id enemy))]
         (let [new-distance (assoc distance :value (move/calc-distance (:x enemy) (:y enemy) (:x player) (:y player)))]
           (when (not= distance new-distance)
             (clara/retract! distance)
             (clara/insert-unconditional! new-distance))))}
      ->session
      init))

(defn tick [session counter]
  (-> session
      (clara/retract (clara/query session :get-game))
      (clara/insert (->Game counter))
      clara/fire-rules))

(defn bench [iterations]
  (time
    (loop [session initial-session
           counter 0]
      (if (= counter iterations)
        session
        (recur (tick session counter) (inc counter))))))

