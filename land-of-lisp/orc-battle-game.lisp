;; The Orc Battle Game
;;
;; You're a knight surrounded by 12 monsters, use your superior wits
;; and sword-fighting skills to maneuver and battle with orcs, hydras,
;; and other nasty enemies.

;; Global variables for players and monsters
(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil) ;; list of our current monsters
(defparameter *monster-builders* nil) ;; list of functions to create each type of monster
(defparameter *monster-num* 12)

;; defin a function called orc-batlle. Initialise the monsters and
;; start the game loop. Determine the victor after the game has ended
;; and print the ending messages.
(defun orc-batlle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congradulations! You have vanquished all of your foes.")))

(defun game-loop ()
  (unless (or (player-dead)
	      (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*)) 15)))
      (unless (monsters-dead)
	(show-monsters)
	(player-attack)))
    (fresh-line)
    (map 'list (lambda (m)
		 (or (monster-dead m)
		     (monster-attack m)))
	 *monsters*)
    (game-loop)))

;; Player management functions. Managing the player's attributes
;; (health, agility, and strength)
(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with the health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
		    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
	 (princ "Your double swing has a strength of ")
	 (princ x)
	 (fresh-line)
	 (monster-hit (pick-monster) x)
	 (unless (monsters-dead)
	   (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
		 (unless (monsters-dead)
		   (monster-hit (random-monster) 1))))))

(defun randval (n)
  (1+ (random (max 1 n))))

(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
	(random-monster)
	m)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x)
		  (>= x 1)
		  (<= x *monster-num*)))
	(progn (princ "That is not a valid monster number.")
	       (pick-monster))
	(let ((m (aref *monsters* (1- x))))
	  (if (monster-dead m)
	      (progn (princ "That monster is already dead.")
		     (pick-monster))
	      m)))))

;; Monster management functions
(defun init-monsters ()
  (setf *monsters* (map 'vector (lambda (x)
				  (declare (ignore x))
				  (funcall (nth (random (length *monster-builders*))
						*monster-builders*)))
			(make-array *monster-num*))))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list (lambda (m)
		 (fresh-line)
		 (princ "    ")
		 (princ (incf x))
		 (princ ". ")
		 (if (monster-dead m)
		     (princ "**dead**")
		     (progn (princ "(Health=")
			    (princ (monster-health m))
			    (princ ") ")
			    (monster-show m))))
	 *monsters*)))

(defstruct monster (health (randval 10)))

(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princ "You killed the ")
	     (princ (type-of m))
	     (princ "! "))
      (progn (princ "You hit the ")
	     (princ (type-of m))
	     (princ ", knocking off ")
	     (princ x)
	     (princ " health points!"))))

(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

(defmethod monster-attack (m))


