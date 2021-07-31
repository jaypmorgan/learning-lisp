;; Different types of streams
;;
;; Console streams -- communication with the repl
;; File streams -- read and write to files
;; Socket streams -- communicate with other computers on a network
;; String streams -- send and receive text in lisp

;; checking if a stream is a valid output stream
(output-stream-p *standard-output*) ;; => T

;; writing to the output stream one character at a time
(write-char #\x *standard-output*)

;; input streams
;; checking validity
(input-stream-p *standard-input*) ;; => T

;; reading a character
(read-char *standard-input*)

;; working with files
;; best way to create a file stream is with `with-open-file'
(with-open-file (my-stream "data.txt" :direction :output)
  (print "my data" my-stream))

(with-open-file (my-stream "data.txt" :direction :input)
  (read my-stream)) ;; => "my data"

;; a more complicated example with alists
(let ((animal-noises '((dog . woof)
		       (cat . meow))))
  (with-open-file (my-stream "animal-noises.txt" :direction :output)
    (print animal-noises my-stream)))

(with-open-file (my-stream "animal-noises.txt" :direction :input)
  (read my-stream))

;; with-open-file can take more arguments such as :if-exists
(with-open-file (my-stream "data.txt" :direction :output :if-exists :error)
  (print "my data" my-stream))

;; or supersede (replace contents)
(with-open-file (my-stream "data.txt" :direction :output :if-exists :supersede)
  (print "my data" my-stream))
