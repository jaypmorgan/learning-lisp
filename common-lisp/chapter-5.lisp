;; 5.1 Rewrite function POOR-STYLE to create a new local variable Q
;; using LET, instead of use SETF to change P. Call your new function
;; GOOD-STYLE.

;; POOR-STYLE for reference

(defun poor-style (p)
  (setf p (+ p 5))
  (list 'result 'is p))

(defun good-style (p)
  (let ((q (+ p 5)))
    (list 'result 'is q)))

(good-style 5)

;; 5.6 This keyboard exercise is about dice. We will start with a
;; function to throw one die and end up with a program to play
;; craps. Be sure to include a documentation string for each function
;; you write.

;; a. Write a function THROW-DIE that returns a random number from 1
;; to 6, inclusive. Remember that (RANDOM 6) will pick numbers from 0
;; to 5. THROW-DIE doesn't need any inputs, so its argument list
;; should be NIL.

(defun throw-die ()
  "Simulate a throw of a 6-sided die and return the result"
  (1+ (random 6)))

;; b. Write a function THROW-DICE that throws two dice and returns a
;; list of two numbers: the value of the first die and value of the
;; second. We'll call this list a "throw." For example, (THROW-DICE)
;; might return the throw (3 5), indicating that the first die was a 3
;; and the second was a 5.

(defun throw-dice ()
  "Simulate the throwing of two 6-sided dice and return a list of results"
  (list (throw-die) (throw-die)))

;; c. Throwing two ones is called "snake eyes"; two sizes is called
;; "boxcars." Write predicates SNAKE-EYES-P and BOXCARS-P that takes a
;; throw as input and returns T if the throw is equal (1 1) or (6 6),
;; respectively.

(defun snake-eyes-p (throws)
  "Return T if both dice values are 1"
  (and (equal (car throws) 1)
       (equal (cadr throws) 1)))

(defun boxcars-p (throws)
  "Return T if both dice values are 6"
  (and (= (car throws) 6)
       (= (cadr throws) 6)))

;; d. In playing craps, the first throw of the dice is crucial. A
;; throw of 7 or 11 is an instant in. A throw of 2, 3, or 12 is an
;; instant loss (American casino rules). Write predicates
;; INSTANT-WIN-P and INSTANT-LOSS-P to detect these conditions. Each
;; should take a throw as input.

(defun instant-win-p (throws)
  (let ((throws (apply #'+ throws)))
    (or (= throws 7) (= throws 11))))

(defun instant-loss-p (throws)
  (let ((throws (apply #'+ throws)))
    (or (= throws 2)
	(= throws 3)
	(= throws 12))))

;; e. Write a function SAY-THROW that takes a throw as input and
;; returns either the sum of the two dice or the symbol SNAKE-EYES or
;; BOXCARS if the sum is 2 or 12. (SAY-THROW '(3 4)) should return
;; 7. (SAY-THROW '(6 6)) should return BOXCARS.

(defun say-throw (throws)
  (cond ((snake-eyes-p throws) 'snake-eyes)
	((boxcars-p throws) 'boxcars)
	(t (apply #'+ throws))))

;; If you don't win or lose on the first throw of the dice, the value
;; you threw becomes your "point," which will be explained
;; shortly. Write a function (CRAPS) that produces the following sort
;; of behaviour. Your solution should make use of the functions you
;; wrote in the previous steps.

;; > (craps)
;; (THROW 1 AND 1 -- SNAKE-EYES -- YOU LOSE)
;; > (craps)
;; (THROW 3 AND 4 -- 7 -- YOU WIN)
;; > (craps)
;; (THROW 2 AND 4 -- YOUR POINT IS 6)

(defun craps ()
  (let* ((throws (throw-dice))
	 (result (say-throw throws))
	 (win-state (instant-win-p throws))
	 (lose-state (instant-loss-p throws)))
    (cond (win-state `(throw ,(car throws) and ,(cadr throws) -- ,result -- you win))
	  (lose-state `(throw ,(car throws) and ,(cadr throws) -- ,result -- you lose))
	  (t `(throw ,(car throws) and ,(cadr throws) -- your point is ,result)))))

;; g. Once a point has been established, you continue throwing the
;; dice until you either win by making the point again or lose by
;; throwing a 7. Write the function TRY-FOR-POINT that simulates this
;; part of the game, as follows:

;; > (try-for-point 6)
;; (THROW 3 AND 5 -- 8 -- THROW AGAIN)
;; > (try-for-point 6)
;; (THROW 5 AND 1 -- 6 -- YOU WIN)
;; > (craps)
;; (THROW 3 AND 6 -- YOUR POINT IS 9)
;; (try-for-point 9)
;; (THROW 6 AND 1 -- 7 -- YOU LOSE)

(defun try-for-point (point)
  (let* ((new-throw (throw-dice))
	 (result (apply #'+ new-throw))
	 (is-seven-p (= result 7))
	 (initial `(throw ,(car new-throw) and ,(cadr new-throw) -- ,result --)))
    (cond (is-seven-p (append initial '(you lose)))
	  ((= result point) (append initial '(you win)))
	  (t (append initial '(throw again))))))
