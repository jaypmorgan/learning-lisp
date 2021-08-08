;; implementation of the Newton-Raphson algorithm for approximating
;; the square root of a number using lazy evaluation seen in
;; https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf

(defun next (n x)
  (/ (+ x (/ n x)) 2))

;; an understanding of how to perform lazy calling
;; https://common-lisp.net/project/clazy/ it seems we can quickly
;; implement lazy-ness by making use of a macro to return a lambda
;; macro that 'promises' to deliver the result when forced (which
;; itself is a simple function that calls the thunk. Instead of using
;; car and cdr in the 'within' function, we've gone ahead and made a
;; head and tail. The head function simply returns the first element
;; of the lazy sequence, while tail forces evaluation of the next
;; element in the stream which will be the delayed expression.

(defmacro delay (expr)
  `(lambda () ,expr))

(defun force (thunk)
  (funcall thunk))

(defun head (lazy-stream)
  (car lazy-stream))

(defun tail (lazy-stream)
  (force (cdr lazy-stream)))

(defun htail (lazy-stream)
  (head (tail lazy-stream)))

(defun repeat (f a)
  (cons a (delay (repeat f (funcall f a)))))

(defun within (eps approximations)
  (labels ((always-number (fn lst)
	     (if (eq (funcall fn lst) nil) 0 (funcall fn lst))))
    (if (<= (abs (- (always-number #'head approximations)
		    (always-number #'htail approximations)))
	    eps)
	(htail approximations)
	(within eps (tail approximations)))))

(defun square-root (a0 eps n)
  (labels ((partial-n (x) (next n x)))
    (within eps (repeat #'partial-n a0))))
