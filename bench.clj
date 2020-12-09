(defmulti task first)

(defmethod task :default
  [[task-name]]
  (println "Unknown benchmark:" task-name)
  (System/exit 1))

(require '[benchmark.simple])
(defmethod task "simple"
  [_]
  (benchmark.simple/bench 10000))

(require '[benchmark.dungeon-crawler.odoyle])
(defmethod task "dungeon"
  [_]
  (benchmark.dungeon-crawler.odoyle/bench 100))

(require '[benchmark.dungeon-crawler.clara])
(defmethod task "dungeon-clara"
  [_]
  (benchmark.dungeon-crawler.clara/bench 100))

(require '[benchmark.people.odoyle])
(defmethod task "people"
  [_]
  (benchmark.people.odoyle/bench))

(require '[benchmark.people.datascript])
(defmethod task "people-datascript"
  [_]
  (benchmark.people.datascript/bench))

(task *command-line-args*)
