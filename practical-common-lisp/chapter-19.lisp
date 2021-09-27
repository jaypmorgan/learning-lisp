(define-condition malformed-log-entry-error (error)
  ((text :initarg :text :reader text)))

(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
      (make-instance 'log-entry)
      (restart-case (error 'malformed-log-entry-error :text text)
	(use-value (value) value)
	(reparse-entry (fixed-text) (parse-log-entry fixed-text)))))

;; using handler-case
;; (defun parse-log-file (file)
;;   (with-open-file (in file :direction :input)
;;     (loop for text = (read-line in nil nil) while text
;;        for entry = (handler-case (parse-log-entry text)
;; 		     (malformed-log-entry-error () nil))
;; 	 when entry collect it)))

(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
    (loop for text = (read-line in nil nil) while text
       for entry (restart-case (parse-log-entry text)
		   (skip-log-entry () nil))
       when entry collect it)))

(defun skip-log-entry (c)
  (let ((restart (find-restart 'skip-log-entry)))
    (when restart (invoke-restart 'skip-log-entry))))

(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error #'skip-log-entry))
    (dolist (log (find-all-logs))
      (analyze-log log))))

(defun analyze-log (log)
  (dolist (entry (parse-log-file log))
    (analyze-entry entry)))
