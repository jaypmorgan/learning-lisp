;; 4.1 Write a function MAKE-EVEN that makes an odd number even by
;; adding one to it. If the input to MAKE-EVEN is already even, it
;; should be returned unchanged.

(defun make-even (n)
  (if (evenp n)
      n
      (1+ n)))

(make-even 1) ;; => 2
(make-even 2) ;; => 2
(make-even 5) ;; => 6
(make-even -2) ;; => -2
(make-even -1) ;; => 0
(make-even 0) ;; => 0

;; 4.2 Write a function FURTHER that makes a positive number larger by
;; adding one to it, and a negative number smaller by subtracting one
;; from it. What does your function do if given the number 0?

(defun further (n)
  (if (plusp n)
      (1+ n)
      (1- n)))

(further 1) ;; => 2
(further -1) ;; => -2
(further 0) ;; => -1 (as predicate is true only if n > 0

;; 4.3 Recall the primitive function NOT: It returns NIL for a true
;; input and T for a false one. Suppose Lisp didn't have a NOT
;; primitive. Show how to write a NOT using just IF and constants (no
;; other functions). Call your function MY-NOT.

(defun my-not (predicate)
  (if predicate
      nil
      t))

(my-not t) ;; => nil
(my-not nil) ;; => t
(my-not (> 3 1)) ;; => nil
(my-not (evenp 1)) ;; => t

;; 4.4 Write a function ORDERED that takes two numbers as input and
;; makes a list of them in ascending order. (ORDERED 3 4) should
;; return the list (3 4). (ORDERED 4 3) should also return (3 4), in
;; other words, the first and second inputs should appear in reverse
;; order when the first is greater than the second.

(defun ordered (a b)
  (if (> a b)
      (list b a)
      (list a b)))

(ordered 3 4) ;; => (3 4)
(ordered 4 3) ;; => (3 4)
(ordered -2 -1) ;; => (-2 -1)

;; 4.5 For each of the following calls to COMPARE, write "1,", "2," or
;; "3" to indicate which clauses of the COND will have a predicate
;; that evaluates to true.

;; the COMPARE function referenced in the question:
(defun compare (x y)
  (cond ((equal x y) 'numbers-are-the-same)
	((< x y) 'first-is-smaller)
	((> x y) 'first-is-bigger)))

;; (compare 9 1) => 3
;; (compare (+ 2 2) 5) => 2
;; (compare 6 (* 2 3)) => 1

;; 4.6 Write a version of the absolute value function MY-ABS using
;; COND instead of IF.

;; the MY-ABS function being referenced in the question
(defun my-abs (x)
  (if (< x 0) (- x) x))

(defun my-abs (x)
  (cond ((< x 0) (- x))
	(t x)))

(my-abs -1) ;; => 1
(my-abs 0) ;; => 0
(my-abs 1) ;; => 1

;; 4.7 For each of the following COND expressions, tell whether the
;; parenthesisation is correct or incorrect. If incorrect, explain
;; where the error lies.

;; (cond (symbolp x) 'symbol
;;       (t 'not-a-symbol))
;;
;; ^ incorrect as the first case in cond should be wrapped in
;; parentheises

;; (cond ((symbolp x) 'symbol)
;;       (t 'not-a-symbol))
;;
;; ^ correct

;; (cond ((symbolp x) ('symbol))
;;       (t 'not-a-symbol)
;;
;; ^ incorrect, the ('symbol) should not be in parenthesis in this
;; context

;; (cond ((symbolp x) 'symbol)
;;       ((t 'not-a-symbol)))
;;
;; ^ incorrect, the base case of 't' is doubly wrapped in parenthesis.

;; 4.8 Write EMPHASIZE3, which is like EMPHASIZE2 but adds the symbol
;; VERY onto the list if it doesn't know how to emphasize it. For
;; example, EMPHASIZE3 of (LONG DAY) should produce (VERY LONG
;; DAY). What does EMPHASIZE3 of (VERY LONG DAY PRODUCE)?

;; EMPHASIZE2 referenced in the question
(defun emphasize2 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
	((equal (first x) 'bad) (cons 'awful (rest x)))
	(t x)))

(defun emphasize3 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
	((equal (first x) 'bad) (cons 'awful (rest x)))
	(t (cons 'very x))))

(emphasize3 '(long day)) ;; => (VERY LONG DAY)
(emphasize3 '(very long day)) ;; => (VERY VERY LONG DAY)

;; 4.9 Type in the following suspicious function definition:
(defun make-odd (x)
  (cond (t x)
	((not (oddp x)) (+ x 1))))

;; What is wrong with this function? Try out the function on the
;; numbers 3, 4, and -2. Rewrite it so it works correctly.

;; the second case is unreachable as it evaluates each case in turn

(defun make-odd (x)
  (cond ((not (oddp x)) (+ x 1))
	(t x)))

;; 4.10 Write a function CONSTRAIN that takes three inputs called X,
;; MAX, and MIN. If X is less than MIN, it should return MIN; if X is
;; greater than MAX, it should return MAX. Otherwise, since X is
;; between MIN and MAX, it should return X. (CONSTRAIN 3 -50 50)
;; should return 3. (CONSTRAIN 92 -50 50) should return 50. Write one
;; version using COND and another using nested IFs.

(defun constrain (x min max)
  (cond ((< x min) min)
	((> x max) max)
	(t x)))

(constrain 3 -50 50) ;; => 3
(constrain 92 -50 50) ;; => 50

(defun constrain (x min max)
  (if (> x min)
      (if (< x max)
	  x
	  max)
      min))

(constrain 3 -50 50) ;; => 3
(constrain 92 -50 50) ;; => 50
(constrain -92 -50 50) ;; => -50

;; 4.11 Write a function FIRSTZERO that takes a list of three numbers
;; as input and returns a word (one of "first," "second," "third," or
;; "none") indicating where the first zero appears in the
;; list. Example: (FIRSTZERO '(3 0 4)) should return SECOND. What
;; happens if you try to call FIRSTZERO with three separate numbers
;; instead of a list of three numbers, as in (FIRST ZERO 3 0 4)?

(defun firstzero (lst)
  (cond ((= (car lst) 0) 'first)
	((= (cadr lst) 0) 'second)
	((= (caddr lst) 0) 'third)
	(t 'none)))

(firstzero '(3 0 4)) ;; => SECOND
;; (firstzero 3 0 4) ;; => ERROR

;; 4.12 Write a function CYCLE that cyclically counts from 1 to
;; 99. CYCLE called with an input of 1 should return 2, with an input
;; of 2 should return 3, with an input of 3 should return 4, and so
;; on. With an input of 99, CYCLE should return 1. That's the cyclical
;; part. Do not try to solve this with 99 COND clauses!

(defun cycle (n)
  (cond ((= n 99) 1)
	((or (< n 0) (> n 99)) 1) ;; wrap anything out of bounds
	(t (1+ n))))

(cycle 1) ;; => 2
(cycle 0) ;; => 1
(cycle 99) ;; => 1
(cycle 98) ;; => 99

;; 4.13 Write a function HOWCOMPUTE that is the inverse of the COMPUTE
;; function described previously. HOWCOMPUTE takes three numbers as
;; input and figures out what operation would produce the third from
;; the first two. (HOWCOMPUTE 3 4 7) should return SUM-OF. (HOWCOMPUTE
;; 3 4 12) should return PRODUCE-OF. HOWCOMPUTE should return the list
;; (BEATS ME) if it can't find a relationship between the first two
;; inputs and the third. Suggest some ways to extend HOWCOMPUTE.

(defun howcompute (a b r)
  (cond ((= (+ a b) r) 'sum-of)
	((= (* a b) r) 'product-of)
	(t 'beats-me)))

(howcompute 3 4 7) ;; => 'sum-of
(howcompute 3 4 12) ;; => 'produce-of
(howcompute 12 3 4) ;; => 'beats-me

;; 4.14 What results do the following expressions produce?
;;
(and 'fee 'fie 'foe) ;; => FOE
(or 'fee 'fie 'foe) ;; => FEE
(or nil 'foe 'nil) ;; => FOE
(and 'fee 'fie nil) ;; => nil
(and (equal 'abc 'abc) 'yes) ;; => YES
(or (equal 'abc 'abc) 'yes) ;; => T


