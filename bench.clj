(defmulti task first)

(defmethod task :default
  [[task-name]]
  (println "Unknown odoyle:" task-name)
  (System/exit 1))

(defmethod task "simple"
  [_]
  (require '[simple.odoyle])
  (time ((resolve 'simple.odoyle/run) 10000)))

(def dungeon-iterations 100)

(defmethod task "dungeon"
  [_]
  (require '[dungeon-crawler.odoyle])
  (time ((resolve 'dungeon-crawler.odoyle/run) dungeon-iterations)))

(defmethod task "dungeon-clara"
  [_]
  (require '[dungeon-crawler.clara])
  (time ((resolve 'dungeon-crawler.clara/run) dungeon-iterations)))

(defmethod task "people"
  [_]
  (require '[people.odoyle])
  (time ((resolve 'people.odoyle/run))))

(defmethod task "people-datascript"
  [_]
  (require '[people.datascript])
  (time ((resolve 'people.datascript/run))))

(defmethod task "todos"
  [_]
  (require '[todos.odoyle])
  (println (time ((resolve 'todos.odoyle/run)))))

(defmethod task "todos-datascript"
  [_]
  (require '[todos.datascript])
  (println (time ((resolve 'todos.datascript/run)))))

(task *command-line-args*)
