(require '[clj-async-profiler.core :as prof])

(defmulti task first)

(defmethod task :default
  [[task-name]]
  (println "Unknown benchmark:" task-name)
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

(defmethod task "dungeon-profile"
  [_]
  (require '[dungeon-crawler.odoyle])
  (prof/profile ((resolve 'dungeon-crawler.odoyle/run) dungeon-iterations))
  (prof/serve-files 8080))

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

(def todo-iterations 5000)

(defmethod task "todos"
  [_]
  (require '[todos.odoyle])
  (time ((resolve 'todos.odoyle/run) todo-iterations)))

(defmethod task "todos-datascript"
  [_]
  (require '[todos.datascript])
  (time ((resolve 'todos.datascript/run) todo-iterations)))

(defmethod task "todos-alt"
  [_]
  (require '[todos.odoyle-alt])
  (time ((resolve 'todos.odoyle-alt/run) todo-iterations)))

(task *command-line-args*)
