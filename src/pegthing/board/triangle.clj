(ns pegthing.board.triangle
  (:gen-class))

(defn get-lineno
  "find line number of a 0-based position when arranged in triangle
   as shown below:
      0
    1  2
  3  4  5"
  [idx]
  (int (Math/ceil (/ (- (Math/sqrt (+ (* 8 idx) 9)) 1) 2))))
; (get-lineno 5)
; (get-lineno 6)


(defn get-lastlineno
  [items]
  (get-lineno (- (count items) 1)))
; (= (get-lastlineno [1 2 3 4 5 6 7]) 4)


(defn fmt-str
  "converts a list of items into triangle format string
   e.g. [1 2 3 4 5] is converted to
       1
     2   3
   4  5"
  ([items]
   (let [maxlines (get-lineno (- (count items) 1))]
     (fmt-str items 1 maxlines "")))
  ([items lineno maxlines boardstr]
   (if (> lineno maxlines)
     boardstr
     (let [curritems (take lineno items)
           remitems  (drop lineno items)
           spacecount (+ (* (- maxlines lineno) 2) 1)
           spaces (clojure.string/join (repeat spacecount " "))
           poses (clojure.string/join "  " curritems)
           nextsstr (str boardstr spaces poses "\n")]
       (recur remitems (+ lineno 1) maxlines nextsstr)))))
; (print (fmt-str (range 1 6)))


(defn get-last-idx
  "Given lineno in triangle of items, get 0-based index of the last item in the line"
  [lineno]
  (- (/ (* lineno (+ lineno 1) ) 2) 1))
; (= (get-last-idx 2) 2)
; (= (get-last-idx 3) 5)
; (= (get-last-idx 4) 9)


(defn get-first-idx
  "Given lineno in triangle of items, get 0-based index of the first item in the line"
  [lineno]
  (/ (* lineno (- lineno 1) ) 2))
; (= (get-first-idx 2) 1)
; (= (get-first-idx 3) 3)
; (= (get-first-idx 4) 6)


(defn get-skip1-connections
  [idx maxlines] ; assumes zero based indexing
  (let [lineno (get-lineno idx)
        lineno_bottom (+ lineno 2)
        idx_line_min (get-first-idx lineno) 
        idx_line_max (get-last-idx lineno)
        idx_left (- idx 2)
        idx_right (+ idx 2)
        idx_bottom_left (+ (* 2 lineno) 1 idx)
        idx_bottom_right (+ idx_bottom_left 2)
        c_left (if (>= idx_left idx_line_min) [idx_left] [])
        c_right (if (<= idx_right idx_line_max) [idx_right] [])
        c_bottom (if (<= lineno_bottom maxlines) [idx_bottom_left idx_bottom_right] [])
        ]
    (set (concat c_left c_right c_bottom))))
; (get-skip1-connections 5 5)
; (get-skip1-connections 3 5) ; 5, 10 ,12
; (get-skip1-connections 3 4) ; only 5
; (get-skip1-connections 8 5)


