;; implementation of examples proposed in https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf

(defun foldr (fn base lst)
  (cond ((eq nil lst) base)
	(t (funcall fn (car lst) (foldr fn base (cdr lst))))))

(defun sum (lst)
  (foldr #'+ 0 lst))

(defun product (lst)
  (foldr #'* 1 lst))
