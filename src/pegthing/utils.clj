(ns pegthing.utils
  (:gen-class))

(defn ask
  ([question] (ask question nil))
  ([question default]
   (print (str question " " (if (= default nil) "" (format "[%s] " default))))
   (flush)
   (let [ans (read-line)]
     (if (empty? ans) default ans))))
; Try below snippet in project repl
; (ask "How are you")

