(defparameter *test-line* "john,dear,42,65,\"West cross, streat\"")

(defun find-delimiter (token string)
  "Find the indexes where the token occurs in a string"
  (let ((indexes nil)
	(x (coerce string 'list))
	(is-string nil))
    (dotimes (n (length x))
      ;; if not in a quoted string add the index of the delimiter
      (cond ((equal #\" (nth n x)) (setf is-string (not is-string)))
	    ((and (equal token (nth n x))
		  (not is-string))
	     (push n indexes))))
    (reverse indexes)))

(defun split-sequence (sequence token)
  "Split a string into a list by the token"
  (cond ((= 0 (length sequence)) nil)
	((= 0 (length (find-delimiter token sequence))) (cons sequence nil))
	(t (let ((indexes (cons 0 (find-delimiter token sequence))))
	     (cons (subseq sequence (first indexes) (second indexes))
		   (split-sequence (subseq sequence (1+ (second indexes))) token))))))

(defun read-csv-contents (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
	  while line
	  collect (split-sequence line #\,))))

(defun read-csv (filename)
  (let* ((contents (read-csv-contents filename))
	 (nrows (1- (length contents)))
	 (ncols (length (car contents))))
    ;; map over the different columns, and get all the data for this
    ;; column as an array.
    (mapcar (lambda (col)
	      (cons (read-from-string (nth col (car contents)))
		    (make-array nrows
				:initial-contents (mapcar (lambda (row)
							    (nth col row))
							  (cdr contents)))))
	    (loop for i below ncols collect i))))

(defparameter *data* (read-csv "data.csv"))

(defmacro select (col data)
  `(cdr (assoc ',col ,data)))

(defun nrows (data)
  (length (cdar data)))

(defun ncols (data)
  (length data))

(select id *data*)

(nrows *data*)
(ncols *data*)
