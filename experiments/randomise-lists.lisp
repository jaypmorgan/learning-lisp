(defun seq (n)
  (loop for i below n collect i))

(defun sample-with-replacement (lst n)
  (loop for i below n collect (nth (random n) lst)))

(defun sample-without-replacement (lst n))


