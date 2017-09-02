(ns pegthing.core
  (:require [pegthing.utils :as utils]
            [pegthing.board.board :as board]
            [pegthing.board.triangle :as triangle])
  (:gen-class))


; NOTES:
; - handle exit command
; - handle invalid move gracefully


(defn start-game
  []
  (let [rowcount (utils/ask "Get ready to play Peg Thing!\nHow many rows?" 5)
        board (board/create-board (Integer. rowcount))]
  board))


(defn pick-peg
  [board]
  (let [boardstr (triangle/fmt-str board)
        pickedpegpos (utils/ask (str "Here's your board:\n" boardstr "Remove which peg?") "e")
        game (board/update-board board (board/postoidx pickedpegpos) board/rem-peg)]
  game))


(defn play-move
  [board]
  (let [boardstr (triangle/fmt-str board)
        move (utils/ask (str "Here's your board:\n" boardstr "Move from where to where? Enter two letters:"))
        nextboard (board/make-move board move)]
  nextboard))


(defn gameloop
  ([] (gameloop (pick-peg (start-game))))
  ([board]
   (if (board/game-playable? board)
     (recur (play-move board))
     (println (str (triangle/fmt-str board) "Game over. Well played, Hope you had fun! :)")))))


(defn -main
  "Let's play Pegthing! :D"
  [& args]
  (gameloop))
