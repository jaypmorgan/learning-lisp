;; Dice of Doom Game
;;
;; Two players occupy spaces on a hexogonal grid, where each space in
;; the grid will have one or more six-sided dice on it. At each turn,
;; a player can choose to attack a neighbouring hexagon, but must have
;; more dice on their hexagon than the other player. If the attack
;; succeeds, all opponents dice is removed from the board with
;; exception of one die, which is moved to the newly won
;; hexagon. After the turn, reinforcements in the form of extra dice
;; are added to the players hexagons one at a time, starting from the
;; upper-left corner, moving across and down. When a player cannot
;; make a turn, the game is ended. The player who occupies the most
;; hexagons wins.

(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
		     collect (list (random *num-players*)
				   (1+ (random *max-dice*))))))

(defun player-letter (n)
  (code-char (+ 97 n)))

(defun draw-board (board)
  (loop for y below *board-size*
	do (progn (fresh-line)
		  (loop repeat (- *board-size* y)
			do (princ " "))
		  (loop for x below *board-size*
			for hex = (aref board (+ x (* *board-size* y)))
			do (format t "~a-~a " (player-letter (first hex)) (second hex))))))
