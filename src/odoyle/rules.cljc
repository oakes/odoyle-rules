(ns odoyle.rules
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]))

(s/def ::rule vector?)

(s/def ::rules (s/map-of qualified-keyword? ::rule))

(defn parse [spec content]
  (let [res (s/conform spec content)]
    (if (= ::s/invalid res)
      (throw (ex-info (expound/expound-str spec content) {}))
      res)))

(defmacro ruleset [rules]
  (list 'quote (parse ::rules rules)))
