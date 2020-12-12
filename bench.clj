(defmulti task first)

(defmethod task :default
  [[task-name]]
  (println "Unknown benchmark:" task-name)
  (System/exit 1))

(defmethod task "simple"
  [_]
  (require '[benchmark.simple])
  (time ((resolve 'benchmark.simple/run) 10000)))

(def dungeon-iterations 100)

(defmethod task "dungeon"
  [_]
  (require '[benchmark.dungeon-crawler.odoyle])
  (time ((resolve 'benchmark.dungeon-crawler.odoyle/run) dungeon-iterations)))

(defmethod task "dungeon-clara"
  [_]
  (require '[benchmark.dungeon-crawler.clara])
  (time ((resolve 'benchmark.dungeon-crawler.clara/run) dungeon-iterations)))

(defmethod task "people"
  [_]
  (require '[benchmark.people.odoyle])
  (time ((resolve 'benchmark.people.odoyle/run))))

(defmethod task "people-datascript"
  [_]
  (require '[benchmark.people.datascript])
  (time ((resolve 'benchmark.people.datascript/run))))

(defmethod task "todos"
  [_]
  (require '[benchmark.todos.odoyle])
  (println (time ((resolve 'benchmark.todos.odoyle/run)))))

(defmethod task "todos-datascript"
  [_]
  (require '[benchmark.todos.datascript])
  (println (time ((resolve 'benchmark.todos.datascript/run)))))

(task *command-line-args*)
