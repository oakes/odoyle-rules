(ns odoyle.rules-parse
  (:require [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.string :as str]))

(defn parse [spec content]
  (let [res (s/conform spec content)]
    (if (= ::s/invalid res)
      (throw (ex-info (expound/expound-str spec content) {}))
      res)))

(defn check-insert-spec
  ([[attr value]]
   (check-insert-spec attr value))
  ([attr value]
   (when-let [spec (s/get-spec attr)]
     (when (= ::s/invalid (s/conform spec value))
       (throw (ex-info (str "Error when checking attribute " attr "\n\n"
                            (expound/expound-str spec value))
                       {}))))))

(defn- add-to-condition [condition field [kind value]]
  (case kind
    :binding (update condition :bindings conj
                     {:field field
                      :sym value
                      :key (keyword value)})
    :value (update condition :nodes conj
                   {:path nil
                    :test-field field
                    :test-value value
                    :children []
                    :successors []
                    :facts {}})))

(defn- parse-condition [{:keys [id attr value opts]}]
  (-> {:bindings [] :nodes [] :opts opts}
      (add-to-condition :id id)
      (add-to-condition :attr attr)
      (add-to-condition :value value)))

(defn parse-rule [rule-name rule]
  (let [{:keys [what-block when-block then-block]} rule
        conditions (mapv parse-condition (:body what-block))
        when-body (:body when-block)
        when-body (if (> (count when-body) 1)
                    (cons 'and when-body)
                    (first when-body))
        then-body (:body then-block)
        syms (->> conditions
                  (mapcat :bindings)
                  (map :sym)
                  set
                  vec)]
    {:rule-name rule-name
     :fn-name (-> (str (namespace rule-name) "-" (name rule-name))
                  (str/replace "." "-")
                  symbol)
     :conditions conditions
     :arg {:keys syms}
     :when-body when-body
     :then-body then-body}))

