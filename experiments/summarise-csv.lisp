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

(defstruct (data-table
	    (:constructor make-data-table (data
					   &aux
					     (nrows (length (cdar data)))
					     (ncols (length data)))))
  data
  (nrows 0 :type integer)
  (ncols 0 :type integer))

(defmethod select ((data data-table) (col symbol))
  "Select a column"
  (assoc col (data-table-data data)))

(defmethod select ((data data-table) (cols list))
  "Select multiple columns"
  (map 'list (lambda (key) (select data key)) cols))

(defmethod names ((data data-table))
  (mapcar #'car (data-table-data data)))

(defmethod rename-col ((data data-table) (org symbol) (new symbol))
  "Rename a column `org' with the column `new'"
  (make-data-table
   (map 'list (lambda (lst)
		(if (eq org (car lst))
		    (cons new (cdr lst))
		    lst))
	(data-table-data data))))

(defmethod rename-col ((data data-table) (org list) (new list))
  "Rename multiple columns and return new dataset"
  (cond ((= (length org) 0) data)
	(t (make-data-table
	    (data-table-data
	     (rename-col
	      (rename-col data (cdr org) (cdr new))
	      (car org) (car new)))))))

(defmethod as-array ((data data-table))
  (let ((rows (data-table-nrows data))
	(cols (data-table-ncols data))
	(data (data-table-data data)))
    (make-array (list cols rows) :initial-contents (map 'list #'cdr data))))

(defmethod mutate ((data data-table) (col symbol) (fn function))
  (let ((newdata (cons (cons col (map 'simple-vector fn (cdr (select data col))))
		       (data-table-data data))))
    (make-data-table
     (mapcar (lambda (key) (assoc key newdata)) (names data)))))

(defmethod summarise ((data data-table) (newcol symbol) (oldcol symbol) (fn function))
  (make-data-table
   (cons (cons newcol (cons (funcall fn (cdr (select data oldcol))) nil)) nil)))

(defparameter *data* (make-data-table (read-csv "data.csv")))

(select *data* 'id)
(select *data* '(id name))

(data-table-nrows *data*)
(data-table-ncols *data*)

(names *data*)

(rename-col *data* 'id 'newid)
(rename-col *data* '(id name) '(newid newname))
(data-table-data *data*)

(as-array *data*)

(setf *data* (mutate *data* 'age #'parse-integer))
(data-table-data *data*)

(defun mean (data)
  (/ (reduce #'+ data) (length data)))

(summarise *data* 'mean-age 'age #'mean)
