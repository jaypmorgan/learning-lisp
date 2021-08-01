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

(defun find-elements (item lst)
  (loop for i below (length lst)
	when (equal (nth i lst) item)
	  collect i))

(find-elements 2 '(1 1 1 2 2 2 3 3))

(defun stratified-sample (pcnt lst)
  "Stratify sample from a list of labels"
  (let* ((classes (remove-duplicates lst))
	 (proportions (mapcar (lambda (cls)
				(/ (length (remove-if-not (lambda (c)
							    (equal c cls))
							  lst))
				   (length lst)))
			      classes))
	 (class-indexes (mapcar (lambda (cls)
				  (find-elements cls lst))
				classes)))
    (mapcar (lambda (indexes prop)
	      (sample-without-replacement indexes (max 1 (round (* (* pcnt prop) (length lst))))))
	    class-indexes
	    proportions)))

(defparameter *targets* '(0 1 0 0 0 1 1 1 1 1))

(stratified-sample 0.10 *targets*)
