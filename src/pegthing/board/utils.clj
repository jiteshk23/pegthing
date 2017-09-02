(ns pegthing.board.utils
  (:gen-class))


(defn chartoidx
  [chr]
  (- (int chr) (int \a)))
; (chartoidx \b)


(defn idxtochar
  [idx]
  (char (+ idx (int \a))))
; (idxtochar 2)


(defn get-mid
  [start end]
  (int (Math/floor (float (/ (+ start end) 2)))))
; (get-mid 5 12)

