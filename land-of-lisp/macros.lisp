(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(let1 foo (+ 2 3)
  (* foo foo))

(defun add (a b)
  (let1 x (+ a b)
    (format t "The sum is ~a" x)
    x))

(macroexpand '(let1 foo (+ 2 3)
	       (* foo foo)))

;; contains bugs!
(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
	     (tail (cdr ,val)))
	 ,yes)
       ,no))

(defun my-length (lst)
  (labels ((f (lst acc)
	     (split lst
		    (f tail (1+ acc)) acc)))
    (f lst 0)))
