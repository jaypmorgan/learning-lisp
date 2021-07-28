(defun seq (n)
  "Generate a sequence from 0 to n"
  (loop for i below n collect i))

(defun take (n lst)
  "Take first n elements from list"
  (subseq lst 0 n))

(defun sample-with-replacement (lst n)
  "Sample from a list n times"
  (loop for i below n collect (nth (random n) lst)))

(defun shuffle (lst)
  "Shuffle a list"
  (cond ((= (length lst) 0) nil)
	(t (let* ((i (length lst))
		 (j (random i))
		 (e (nth j lst)))
	     (cons e (shuffle (remove e lst)))))))

(defun sample-without-replacement (lst n)
  "Sample from a list without duplicating elements"
  (let ((indexes (shuffle (seq (length lst)))))
    (take n indexes)))


