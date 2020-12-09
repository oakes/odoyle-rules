(defmulti task first)

(defmethod task :default
  [[task-name]]
  (println "Unknown benchmark:" task-name)
  (System/exit 1))

(require '[odoyle.benchmark.simple])
(defmethod task "simple"
  [_]
  (odoyle.benchmark.simple/bench 10000))

(require '[odoyle.benchmark.dungeon-crawler.odoyle])
(defmethod task "dungeon"
  [_]
  (odoyle.benchmark.dungeon-crawler.odoyle/bench 100))

(require '[odoyle.benchmark.dungeon-crawler.clara])
(defmethod task "dungeon-clara"
  [_]
  (odoyle.benchmark.dungeon-crawler.clara/bench 100))

(require '[odoyle.benchmark.people.odoyle])
(defmethod task "people"
  [_]
  (odoyle.benchmark.people.odoyle/bench))

(require '[odoyle.benchmark.people.datascript])
(defmethod task "people-datascript"
  [_]
  (odoyle.benchmark.people.datascript/bench))

(task *command-line-args*)
