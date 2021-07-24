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
(defun orc-battle ()
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
(defstruct monster (health (randval 10)))

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

;; The wicked orc! Delivers a strong attack with his club, but
;; otherwise pretty harmless. Every orc has a unique attack lever.
(defstruct (orc (:include monster))	; inherit from basic monster
  (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at your and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))

;; The malicious hydra! A very nasty enemy. It will attack you with
;; its many heads. The hydr's special power is that it can grow a new
;; head during each round of battle, meaning you'll want to deal with
;; it as quickly as possible.
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated hydra falls to the floor!")
      (progn (princ "You lop off ")
	     (princ x)
	     (princ " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head!" )
    (incf (monster-health m))
    (decf *player-health* x)))

;; The slimy slime mold! When it attacks you, it wraps itself around
;; you and you are immobolised.
(defstruct (slime-mold (:include monster))
  (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "!")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

;; The cunning brigand. The smartest of all your foes, he uses his
;; whip or slingshot. His attacks are not powerful, but they are
;; consistent, knocking two points off of you every round.
(defstruct (cunning-brigand (:include monster)))
(push #'make-cunning-brigand *monster-builders*)

(defmethod monster-attack ((m cunning-brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
	   (princ "A brigand hits your with his slingshot, taking off two health points!")
	   (decf *player-health* 2))
	  ((= x *player-agility*)
	   (princ "A brigand catches your leg with his whip, taking off 2 agility points!")
	   (decf *player-agility* 2))
	  ((= x *player-strength*)
	   (princ "A brigand cuts your arm with his whip, taking off 2 strength points!")
	   (decf *player-strength* 2)))))