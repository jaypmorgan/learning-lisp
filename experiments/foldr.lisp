;; implementation of examples proposed in https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf

(defun foldr (fn base lst)
  (cond ((eq nil lst) base)
	(t (funcall fn (car lst) (foldr fn base (cdr lst))))))

(defun sum (lst)
  (foldr #'+ 0 lst))

(defun product (lst)
  (foldr #'* 1 lst))

(defun anytrue (lst)
  (foldr #'logior 0 lst))

(defun alltrue (lst)
  (foldr #'logand 1 lst))

(defun my/append (a b)
  (foldr #'cons b a))

(defun my/length (lst)
  (labels ((my/count (a n)
	     (declare (ignore a))
	     (incf n)))
    (foldr #'my/count 0 lst)))

(defun doubleall (lst)
  (labels ((doubleandcons (n lst)
	     (cons (* 2 n) lst)))
    (foldr #'doubleandcons nil lst)))

;; Tests

(sum '()) ;; => 0
(sum '(1 2 3)) ;; => 6

(product '()) ;; => 1
(product '(1 2 3)) ;; => 6

(anytrue '(0 0 0)) ;; => 0
(anytrue '(1 0 0)) ;; => 1
(anytrue '(1 1 1)) ;; => 1
(anytrue '()) ;; => 0

(alltrue '(0 1 0)) ;; => 0
(alltrue '(1 1 0)) ;; => 0
(alltrue '()) ;; => 1
(alltrue '(1 1 1)) ;; => 1

(my/append '(1 2) '(3 4)) ;; => (1 2 3 4)
(my/append '() '()) ;; => NIL

(my/length '(1 2 3 4)) ;; => 4
(my/length '()) ;; => 0

(doubleall '(1 2 3 4)) ;; => (2 4 6 8)
(doubleall '()) ;; => NIl
