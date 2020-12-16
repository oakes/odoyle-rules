(ns dungeon-crawler.entities)

(def directions [:w :nw :n :ne
                 :e :se :s :sw])
(def velocities [[-1 0] [-1 -1] [0 -1] [1 -1]
                 [1 0] [1 1] [0 1] [-1 1]])
(def tile-size 256)
(def player-spawn-point {:x 2.5 :y 2.5})
(def cols 4)
(def rows 4)

(def spawn-points (for [row (range rows)
                        col (range cols)
                        :let [point {:x (-> row (* 10) (+ 2.5))
                                     :y (-> col (* 10) (+ 2.5))}]
                        :when (not= point player-spawn-point)]
                    point))

(def spawn-data
 [{:attrs {::kind :player
           ::health 20
           ::damage 4}
   :path "characters/male_light.png"
   :mask-size 128
   :instances [player-spawn-point]}
  {:attrs {::kind :ogre
           ::health 8
           ::damage 2}
   :path "characters/ogre.png"
   :mask-size 256
   :instances (->> spawn-points shuffle (take 30))}
  {:attrs {::kind :elemental
           ::health 6
           ::damage 1}
   :path "characters/elemental.png"
   :mask-size 256
   :instances (->> spawn-points shuffle (take 30))}])

(defn ->entity [{:keys [attrs mask-size]} {:keys [x y]}]
  (merge attrs
    {::direction :s
     ::width (/ mask-size tile-size)
     ::height (/ mask-size tile-size)
     ::x x
     ::y y
     ::x-change 0
     ::y-change 0
     ::x-velocity 0
     ::y-velocity 0}))

