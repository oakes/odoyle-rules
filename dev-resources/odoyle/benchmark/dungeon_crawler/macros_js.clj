(ns odoyle.benchmark.dungeon-crawler.macros-js)

(defmacro math
  "Wraps the Math object, calling a method if the provided symbol starts with a
  lower-case letter, or a property if it starts with an upper-case letter."
  [n & args]
  (let [s (str n)
        ^Character l (nth s 0)]
    (if (Character/isUpperCase l)
      (symbol (str 'js "/Math." s))
      (cons (symbol (str 'js "/Math." s)) args))))

