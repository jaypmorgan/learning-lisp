;; Creating arrays
(defparameter x (make-array 3))

;; setting the value using setf
(setf (aref x 1) 'foo)

;; setf sets the value of an index using a generalised reference
;; an example is aref, but others can be second etc.
(setf foo '(a b c))
(second foo)
(setf (second foo) 'z)

(setf foo (make-array 4))
(setf (aref foo 2) '(x y z))
(setf (car (aref foo 2)) (make-hash-table))
(setf (gethash 'zoink (car (aref foo 2))) 5)

;; Hash tables
(defparameter x (make-hash-table))
(gethash 'yup x) ;; no key 'yup exists so NIL is retruned.  two nils
;; are returned, the nil being the value in the hash table, and the
;; second being whether the key was found


(setf (gethash 'yup x) '25)
(gethash 'yup x) ;; returns '25, T.

(defparameter *drink-order* (make-hash-table))
(setf (gethash 'bill *drink-order*) 'double-espresso)
(setf (gethash 'lisa *drink-order*) 'small-drip-coffee)
(setf (gethash 'john *drink-order*) 'medium-latte)

(defparameter *drink-order* (make-hash-table))
(let* ((orders '((bill double-espresso)
		 (lisa small-drip-coffee)
		 (john medium-latte))))
  (mapc (lambda (x) (setf (gethash (car x) *drink-order*) (cadr x)))
	orders))

(gethash 'bill *drink-order*)

;; Returning multiple values
;; we can return multiple values form a function using =values=
(defun foo ()
  (values 3 7))

;; lisp considers the first argument to be most important and so will
;; use it for follow up calculations
(+ (foo) 5) ;; => 8

;; if we want to retrieve both values:
(multiple-value-bind (a b) (foo) (* a b)) ;; => 21



