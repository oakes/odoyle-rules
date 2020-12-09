(ns odoyle.benchmark.dungeon-crawler.macros-java)

(defmacro math
  "Wraps java.lang.Math, calling a method if the provided symbol starts with a
  lower-case letter, or a static field if it starts with an upper-case letter."
  [n & args]
  (let [s (str n)
        ^Character l (nth s 0)]
    (if (Character/isUpperCase l)
      (symbol (str 'Math "/" s))
      (cons (symbol (str 'Math "/" s)) args))))

