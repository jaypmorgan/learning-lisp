;;;; A Sample Macro: do-primes
;;;;
;;;; do-primes provides a looping construct similar to DOTIMES and
;;;; DOLIST except that instead of interating over integers or
;;;; elements of a list, it iterates over successive prime numbers.

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number)
	  never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number
	when (primep n) return n))

;;; example call of the do-primes macro
;;; (do-primes (p 0 19)
;;;   (format t "~d " p))
;;;
;;; which may look like this in a DO construct
;;; (do ((p (next-prime 0) (next-prime (+1 p))))
;;;     ((> p 19))
;;;   (format t "~d " p))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))

(do-primes (p 0 19)
  (format t "~d " p))
