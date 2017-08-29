(ns pegthing.core
  (:gen-class))


; NOTES:
; - handle exit command
; - handle invalid move gracefully


(defn add-peg
  [pos]
  (clojure.string/replace pos #"-" "0"))
; (add-peg "a-")


(defn rem-peg
  [pos]
  (clojure.string/replace pos #"0" "-"))
; (rem-peg "a0")


(def chartoidx #(- (int %) (int \a)))
; (chartoidx \b)


(def idxtochar #(char (+ % (int \a))))
; (idxtochar 2)


(def get-mid #(int (Math/floor (float (/ (+ %1 %2) 2)))))
; (get-mid 5 12)


(defn getlineno
  [idx]
  (int (Math/ceil (/ (- (Math/sqrt (+ (* 8 idx) 9)) 1) 2))))
; (getlineno 5)
; (getlineno 6)
; (getlineno 7)
; (getlineno 8)
; (getlineno 9)
; (getlineno 10)


(defn nopeg?
  [pos]
  (= (get pos 1) \-))
; (nopeg? "a-")


(def haspeg? (complement nopeg?))
; (haspeg? "a0")


(defn init-board
  [nrows]
  (let [npos (/ (* nrows (+ nrows 1)) 2)]
    (assert (<= npos 26))
    (into [] (map #(str (idxtochar %) \0) (range npos)))))
; (init-board 4)


(defn update-board
  [board idx method]
        (assoc board idx (method (get board idx))))
; (update-board (init-board 5) 2 rem-peg)


(defn print-board
  ([items]
   (let [maxlines (getlineno (- (count items) 1))]
     (print-board items 1 maxlines "")))
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
; (print (print-board (init-board 6) 1 4 ""))


(defn get-connections
  [idx maxlines] ; assumes zero based indexing
  (let [lineno (getlineno idx)
        lineno_bottom (+ lineno 2)
        idx_line_min (/ (* lineno (- lineno 1) ) 2)
        idx_line_max (- (/ (* lineno (+ lineno 1) ) 2) 1)
        idx_left (- idx 2)
        idx_right (+ idx 2)
        idx_bottom_left (+ (* 2 lineno) 1 idx)
        idx_bottom_right (+ idx_bottom_left 2)
        c_left (if (>= idx_left idx_line_min) [idx_left] [])
        c_right (if (<= idx_right idx_line_max) [idx_right] [])
        c_bottom (if (<= lineno_bottom maxlines) [idx_bottom_left idx_bottom_right] [])
        ]
    (set (concat c_left c_right c_bottom))))
; (print (print-board (init-board 5)))
; (get-connections 5 5)
; (get-connections 3 5) ; 5, 10 ,12
; (get-connections 3 4) ; only 5
; (get-connections 8 5)


(defn get-maxlines
  [board]
  (getlineno (- (count board) 1)))
; (= (get-maxlines (init-board 5)) 5)


(defn valid-move?
  [board move]
  (let [[start end] (map chartoidx move)
        startpos (get board start)
        endpos (get board end)
        smaller (min start end)
        bigger (max start end)
        maxlines (get-maxlines board)
        connections (get-connections smaller maxlines)]
    (and (some #(= % bigger) connections) (haspeg? startpos) (nopeg? endpos))))


(defn make-move
  [board move]
  (assert (valid-move? board move) (format "invalid move: %s" move))
  (let [[start end] (map chartoidx move)
        mid (get-mid start end)
        step1 (update-board board start rem-peg)
        step2 (update-board step1 mid rem-peg)
        step3 (update-board step2 end add-peg)]
      step3))
; (def game (update-board (init-board 5) (chartoidx \e) rem-peg))
; (print (print-board game))
; (print (print-board (make-move game "hc")))
; (print (print-board (make-move game "le")))
; (print (print-board (make-move (make-move game "le") "nl")))


(def postoidx #(chartoidx (first %)))
; (postoidx "c0")


(defn pos-playable?
  [board pos]
  (let [maxlines (get-maxlines board)
        idx (postoidx pos)
        connections (get-connections idx maxlines)
        idxhaspeg? #(haspeg? (get board %))
        idxnopeg?  #(nopeg? (get board %))
        haspeg_connections (filter idxhaspeg? connections)
        nopeg_connections (filter idxnopeg? connections)
        haspeg_mids (map #(get-mid idx %) haspeg_connections)
        nopeg_mids (map #(get-mid idx %) nopeg_connections)]
    (boolean ( or (and (idxhaspeg? idx) (some idxhaspeg? nopeg_mids)) (and (idxnopeg? idx) (some idxhaspeg? haspeg_mids))))))


(defn game-playable?
  [board]
  (boolean (some #(pos-playable? board %) board)))

; (def game1 (update-board (init-board 5) (chartoidx \e) rem-peg))
; (print (print-board game1))
; (game-playable? game1)

; (def game2 (update-board (init-board 4) (chartoidx \e) rem-peg))
; (print (print-board game2))
; (game-playable? game2)


(defn ask
  ([question] (ask question nil))
  ([question default]
   (print (str question " " (if (= default nil) "" (format "[%s] " default))))
   (flush)
   (let [ans (read-line)]
     (if (empty? ans) default ans))))
; Try below in project repl
; (ask "How are you")


(defn start-game
  []
  (let [rowcount (ask "Get ready to play Peg Thing!\nHow many rows?" 5)
        board (init-board (Integer. rowcount))]
  board))


(defn pick-peg
  [board]
  (let [boardstr (print-board board)
        pickedpegpos (ask (str "Here's your board:\n" boardstr "Remove which peg?") "e")
        game (update-board board (postoidx pickedpegpos) rem-peg)]
  game))


(defn play-move
  [board]
  (let [boardstr (print-board board)
        move (ask (str "Here's your board:\n" boardstr "Move from where to where? Enter two letters:"))
        nextboard (make-move board move)]
  nextboard))


(defn gameloop
  ([] (gameloop (pick-peg (start-game))))
  ([board]
   (if (game-playable? board)
     (recur (play-move board))
     (println (str (print-board board) "Game over. Well played, Hope you had fun! :)")))))


(defn -main
  "Let's play Pegthing! :D"
  [& args]
  (gameloop))
