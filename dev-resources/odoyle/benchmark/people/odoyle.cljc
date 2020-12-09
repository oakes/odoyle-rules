(ns odoyle.benchmark.people.odoyle
  (:require [odoyle.rules :as o #?(:clj :refer :cljs :refer-macros) [ruleset]]
            [odoyle.benchmark.people.core :as core]))

(defn init []
  (as-> (o/->session) $
        (reduce o/add-rule $
          (o/ruleset
            {::get-ivan
             [:what
              [id ::core/name "Ivan"]
              [id ::core/last-name last-name]
              [id ::core/age age]
              [id ::core/sex :male]]}))
        (reduce (fn [session person]
                  (o/insert session (:db/id person) person))
                $ core/people20k)))

(def initial-session (init))

(defn query [session]
  (o/query-all session ::get-ivan))

(defn bench []
  (time (query initial-session)))

