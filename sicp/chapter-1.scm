;; Exercise 1.1

(define (check-answer guess result)
  (assert (cond ((number? result) (= guess result))
		((boolean? result) (eq? guess result))
		(else #f))))

(check-answer 10 10)
(check-answer 12 (+ 5 3 4))
(check-answer 8  (- 9 1))
(check-answer 3  (/ 6 2))
(check-answer 6  (+ (* 2 4) (- 4 6)))
(define a 3)  ; #<void>
(define b (+ a 1)) ; #<void>
(check-answer 19 (+ a b (* a b)))
(check-answer #f (= a b))
(check-answer 4  (if (and (> b a) (< b (* a b)))
		     b
		     a))
(check-answer 16 (cond ((= a 4) 6)
		       ((= b 4) (+ 6 7 a))
		       (else 25)))
(check-answer 6 (+ 2 (if (> b a) b a)))
(check-answer 16 (* (cond ((> a b) a)
			  ((< a b) b)
			  (else -1))
		    (+ a 1)))

;; Exercise 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 7 2)))

;; Exercise 1.3

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sum-of-largest x y z)
  (let ((sorted (sort > (list x y z))))
    (sum-of-squares (car sorted) (cadr sorted))))

;; Exercise 1.4

;; Given that our model of evaluation allows for operators to be
;; compound expressions, the procedure selects either an addition or
;; subtraction of parameters a, b. If b is above 0, the procedure adds
;; these two parameters together, else they will be subtracted.

;; Exercise 1.5

;; If the interpreter is using normal-order evaluation, then all terms
;; will be fully expanded until only primitives are left then
;; reduced. Given that the second term in the test is an infinite
;; recursive call, a normal-order evaluator would not terminate the
;; expansion process.

;; Exercise 1.6

;; As `new-if` is not a special form with a different rule for
;; evaluation, each argument of function call is evaluated. This means
;; that both the consequent and alternative of if-statement are
;; erroneously called. In terms of sqrt-iter, an infinite recursion
;; occurs.

;; Exercise 1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;; original good-enough?

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x) (sqrt-iter 1.0 x))

;; example for a very small number

(sqrt 1e-50)

;; example for a very large number

;; (sqrt 1e+50) ;; crashes

;; better good-enough?

(define (sqrt-iter old-guess guess x)
  (if (good-enough? old-guess guess)
      guess
      (sqrt-iter guess (improve guess x) x)))

(define (good-enough? old-guess guess)
  (< (abs (- 1 (/ old-guess guess))) 0.001))

(define (sqrt x) (sqrt-iter 100.0 1.0 x))

(sqrt 1e+50)

;; Exercise 1.8

(define (cube x) (* x (square x)))

(define (cube-root x)
  (define (cube-root-iter guess x)
    (if (good-enough? guess x)
	guess
	(cube-root-iter (improve guess x) x)))
  (define (good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess x)
    (/ (+ (* 2 guess) (/ x (square guess))) 3))
  (cube-root-iter 1.0 x))
