(ns pegthing.board.board
  (:require [pegthing.board.utils :as utils]
            [pegthing.board.triangle :as triangle])
  (:gen-class))


(defn add-peg
  [pos]
  (clojure.string/replace pos #"-" "0"))
; (add-peg "a-")


(defn rem-peg
  [pos]
  (clojure.string/replace pos #"0" "-"))
; (rem-peg "a0")


(defn update-board
  [board idx method]
  (assoc board idx (method (get board idx))))
; (update-board (create-board 5) 2 rem-peg)


(defn nopeg?
  [pos]
  (= (get pos 1) \-))
; (nopeg? "a-")


(def haspeg? (complement nopeg?))
; (haspeg? "a0")


(defn create-board
  [nrows]
  (let [npos (/ (* nrows (+ nrows 1)) 2)]
    (assert (<= npos 26))
    (into [] (map #(str (utils/idxtochar %) \0) (range npos)))))
; (create-board 4)


(defn valid-move?
  [board move]
  (let [[start end] (map utils/chartoidx move)
        startpos (get board start)
        endpos (get board end)
        smaller (min start end)
        bigger (max start end)
        maxlines (triangle/get-lastlineno board)
        connections (triangle/get-skip1-connections smaller maxlines)]
    (and (some #(= % bigger) connections) (haspeg? startpos) (nopeg? endpos))))


(defn make-move
  [board move]
  (assert (valid-move? board move) (format "invalid move: %s" move))
  (let [[start end] (map utils/chartoidx move)
        mid (utils/get-mid start end)
        step1 (update-board board start rem-peg)
        step2 (update-board step1 mid rem-peg)
        step3 (update-board step2 end add-peg)]
    step3))
; (def game (update-board (create-board 5) (utils/chartoidx \e) rem-peg))
; (print (triangle/fmt-str game))
; (print (triangle/fmt-str (make-move game "hc")))
; (print (triangle/fmt-str (make-move game "le")))
; (print (triangle/fmt-str (make-move (make-move game "le") "nl")))


(defn postoidx
  [pos]
  (utils/chartoidx (first pos)))
; (postoidx "c0")


(defn pos-playable?
  [board pos]
  (let [maxlines (triangle/get-lastlineno board)
        idx (postoidx pos)
        connections (triangle/get-skip1-connections idx maxlines)
        idxhaspeg? #(haspeg? (get board %))
        idxnopeg?  #(nopeg? (get board %))
        haspeg_connections (filter idxhaspeg? connections)
        nopeg_connections (filter idxnopeg? connections)
        haspeg_mids (map #(utils/get-mid idx %) haspeg_connections)
        nopeg_mids (map #(utils/get-mid idx %) nopeg_connections)]
    (boolean ( or (and (idxhaspeg? idx) (some idxhaspeg? nopeg_mids)) (and (idxnopeg? idx) (some idxhaspeg? haspeg_mids))))))


(defn game-playable?
  [board]
  (boolean (some #(pos-playable? board %) board)))

; (def game1 (update-board (create-board 5) (utils/chartoidx \e) rem-peg))
; (print (triangle/fmt-str game1))
; (game-playable? game1)

; (def game2 (update-board (create-board 4) (utils/chartoidx \e) rem-peg))
; (print (triangle/fmt-str game2))
; (game-playable? game2)


