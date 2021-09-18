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

;; Warning! Still contains a bug!
(defmacro split (val yes no)
  `(let1 x ,val
	 (if x (let ((head (car x))
		     (tail (cdr x)))
		 ,yes)
	     ,no)))

;; finally safe to use
(defmacro split (val yes no)
  (let1 g (gensym)
	`(let1 ,g ,val
	       (if ,g (let ((head (car ,g))
			    (tail (cdr ,g)))
			,yes)
		   ,no))))

;; Recurse macro
(defun pairs (lst)
  (labels ((f (lst acc)
	     (split lst
		    (if tail
			(f (cdr tail) (cons (cons head (car tail)) acc))
			(reverse acc))
		    (reverse acc))))
    (f lst nil)))

(recurse (n 9)
	 (fresh-line)
	 (if (zerop n)
	     (princ "Lift-off!")
	     (progn (princ n)
		    (self (1- n)))))
