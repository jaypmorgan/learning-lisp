;; write a function that takes any two inputs and makes a list of them
;; using cons
(defun my-cons (a b)
  (cons a (cons b nil)))

(my-cons '1 '2)

;; 2.21 Write a unction that takes four inputs and returns a
;; two-element nested list. The first element should be a list of the
;; first two inputs, and teh second element a list of the last two
;; inputs

(defun my-four-cons (a b c d)
  (cons
   (my-cons a b)
   (cons (my-cons c d) nil)))

(my-four-cons 1 2 3 4)

;; 2.22 Suppose we wanted to make a function called DUO-CONS that
;; added two elements to the front of a list. DUO-CONS would be a
;; function of three inputs. For example, if the inputs were the
;; symbol PATRICK, the symbol SEYMOUR, and the list (MARVIN), DUO-CONS
;; would return the list (PATRICK SEYMOUR MARVIN).

(defun duo-cons (el1 el2 lst)
  (cons el1 (cons el2 lst)))

(duo-cons 'patrick 'seymour '(marvin))

;; 2.23 TWO-DEEPER is a function that surrounds its input with two
;; levels of paranthesis. TWO-DEEPER of MOO is ((MOO)). TWO-DEEPER of
;; (BOW WOW) is (((BOW WOW))). Show how to write TWO-DEEPER using
;; LIST. Write another using cons.

(defun two-deeper-list (x)
  (list (list x)))

(two-deeper-list 'moo)
(two-deeper-list '(bow wow))

(defun two-deeper-cons (x)
  (cons (cons x nil) nil))

(two-deeper-cons 'moo)
(two-deeper-cons '(bow wow))

;; 2.24 What built-in lisp function would extract the symbol NIGHT from the list (((GOOD)) ((NIGHT))).
;; Answer: caaadr. Example:
(defparameter x '(((good)) ((night))))
(caaadr x) ;; => NIGHT

;; 2.29 Write a function UNARY-ADD 1 that increases a unary number by one.
(defun unary-add-one (x)
  (cons 'x x))

(unary-add-one nil)
(unary-add-one '(x))
(unary-add-one '(x x))

;; 2.30 What does the CDDR function do to unary numbers?
;; Answer: return the rest of tally marks from 3 onwards
(cddr '(x x x))
(cddr '(x x x x))

;; 2.31 Write a UNARY-ZEROP predicate.
(defun unary-zerop (tally)
  ;; Returns T if zero length else NIL
  (not (car tally)))

(unary-zerop '(x))
(unary-zerop nil)
(unary-zerop '(x x))

;; 2.32 Write a UNARY-GREATERP predicate, analoguous to the > predicte
;; of ordinary numbers.
(defun unary-greaterp (tal1 tal2)
  (> (length tal1) (length tal2)))

(unary-greaterp '(x x) '(x x x)) ;; => NIL
(unary-greaterp '(x x x) '(x))   ;; => T
(unary-greaterp '(x) nil)        ;; => T

;; 2.33 CAR can be viewed as a predicate on unary numbers. Instead of
;; returning T or NIL, CAR returns X or NIL. Remember that X or any
;; other non-NIL object is taken as true in lisp. What question about
;; a unary number does CAR answer?
;; Answer: If the unary number is greater than zero.
(car '(x)) ;; => X
(car nil)  ;; => NIL
(car '(x x)) ;; => X

;; 2.34 Write an expression involing cascaded calls to cons to
;; construct the dotted list (A B C . D)
(defun cascaded-cons ()
  (cons 'A (cons 'B (cons 'C 'D))))

(cascaded-cons)

;; 2.35 Draw the dotted list ((A . B) (C . D)) in cons cells
;; notation. Write an expression to construct this list
(cons (cons 'A 'B) (cons (cons 'C 'D) nil))
