(load "summarise-csv.lisp")
(ql:quickload :cl-json)

(defparameter *test-line* "{\"id\": 1, \"name\": \"john\", \"age\": 3}")

(defun read-jsonl-contents (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
	  while line
	  collect (with-input-from-string (s line)
		    (json:decode-json s)))))

(defun get-header (jsonlist)
  (let ((item (car jsonlist)))
    (map 'list #'car item)))

(defun read-jsonl (filename)
  (let* ((contents (read-jsonl-contents filename))
	 (header (get-header contents)))
    (map 'list (lambda (heading)
		 (cons heading (mapcar (lambda (row)
					 (loop for item in row
						  when (eq (car item) heading)
						  return (cdr item)))
				    contents)))
	 header)))

(defparameter *data*  (read-jsonl "data.jsonl"))
