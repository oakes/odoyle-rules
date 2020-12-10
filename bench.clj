(defmulti task first)

(defmethod task :default
  [[task-name]]
  (println "Unknown benchmark:" task-name)
  (System/exit 1))

(require '[benchmark.simple])
(defmethod task "simple"
  [_]
  (time (benchmark.simple/run 10000)))

(def dungeon-iterations 100)

(require '[benchmark.dungeon-crawler.odoyle])
(defmethod task "dungeon"
  [_]
  (time (benchmark.dungeon-crawler.odoyle/run dungeon-iterations)))

(require '[benchmark.dungeon-crawler.clara])
(defmethod task "dungeon-clara"
  [_]
  (time (benchmark.dungeon-crawler.clara/run dungeon-iterations)))

(require '[benchmark.people.odoyle])
(defmethod task "people"
  [_]
  (time (benchmark.people.odoyle/run)))

(require '[benchmark.people.datascript])
(defmethod task "people-datascript"
  [_]
  (time (benchmark.people.datascript/run)))

(task *command-line-args*)
