(defmulti task first)

(defmethod task :default
  [[task-name]]
  (println "Unknown odoyle:" task-name)
  (System/exit 1))

(defmethod task "simple"
  [_]
  (require '[odoyle.simple])
  (time ((resolve 'odoyle.simple/run) 10000)))

(def dungeon-iterations 100)

(defmethod task "dungeon"
  [_]
  (require '[odoyle.dungeon-crawler.odoyle])
  (time ((resolve 'odoyle.dungeon-crawler.odoyle/run) dungeon-iterations)))

(defmethod task "dungeon-clara"
  [_]
  (require '[odoyle.dungeon-crawler.clara])
  (time ((resolve 'odoyle.dungeon-crawler.clara/run) dungeon-iterations)))

(defmethod task "people"
  [_]
  (require '[odoyle.people.odoyle])
  (time ((resolve 'odoyle.people.odoyle/run))))

(defmethod task "people-datascript"
  [_]
  (require '[odoyle.people.datascript])
  (time ((resolve 'odoyle.people.datascript/run))))

(defmethod task "todos"
  [_]
  (require '[odoyle.todos.odoyle])
  (println (time ((resolve 'odoyle.todos.odoyle/run)))))

(defmethod task "todos-datascript"
  [_]
  (require '[odoyle.todos.datascript])
  (println (time ((resolve 'odoyle.todos.datascript/run)))))

(task *command-line-args*)
