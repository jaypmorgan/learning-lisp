(defvar *some-list* '(1 2 3 "hello"))

(loop for cons on *some-list*
      do (format t "~\a" (car cons))
      when (cdr cons) do (format t ", "))

;; same thing in a single format instruction
(format t "~{~a~^, ~}" *some-list*)

;; floating point number ~$ directive
(format t "~$~%" pi)
(format t "~5$~%" pi)

;; consume a prefix parameter with v
(format t "~v$" 3 pi)
(format t "~#$" pi) ;; conditional formatting

(format t "~,5f" pi)

;; formatting modifiers with :
(format t "~d" 100000)
(format t "~:d" 100000)
(format t "~@d" 100000)
(format t "~:@d" 100000)

;; aesthetic (human-readable) form with ~a
(format nil "The value is: ~a" 10)
(format nil "The value is: ~a" "foo")
(format nil "The value is: ~a" (list 1 2 3))

;; ~S to format into something that can be used by READ
(format nil "The value is: ~s" 10)
(format nil "The value is: ~s" "foo")
(format nil "The value is: ~s" (list 1 2 3))

;; ~C
(format t "~&Syntax error. Unexpected character: ~c" #\space)
(format t "~&Syntax error. Unexpected character: ~:c" #\space)
(format t "~&Syntax error. Unexpected character: ~@c" #\space)

;; formatting date with ~d and modifiers
(format nil "~4,'0d-~2,'0d-~2,'0d" 2006 6 1)

;; 4 places: 1 = width, 2 = padding char, 3 = sep char, 4 = digits per group
(format nil "~,,'.,4:d" 100000000)
;; unspecified parameters need to be held with commas

;; ~X, ~O, ~B for hex, octal, and binary formatting of numbers
(format nil "~X" 10000)
(format nil "~O" 10000)
(format nil "~B" 10000)

(format nil "~2,10,'0,:R" 2)

;; english-language directives
(format nil "~r" 1234) ;; => "one thousand two hundred and thirty-four"

;; ~:R => an ordinal
(format nil "~:R" 1234)

;; pluralised
(format nil "file~P" 1) ;; => file
(format nil "file~P" 10) ;;=> files

;; reprocess the same argument
(format nil "~r file~:P" 1) ;;=> one file

(format nil "~r famil~:@P" 1)

;; control case with ~( ~)
(format nil "~(~a~)" "tHe Quick BROWN FoX") ;; => all lower
(format nil "~@(~a~)" "tHe Quick BROWN FoX") ;; => capitalise the first char
(format nil "~:(~a~)" "tHe Quick BROWN FoX") ;; => title case
(format nil "~:@(~a~)" "tHe Quick BROWN FoX") ;; => all upper

;; conditional formatting
(format nil "~[cero~;uno~;dos~]" 0)
(format nil "~[cero~;uno~;dos~]" 1)
(format nil "~[cero~;uno~;dos~]" 2)
(format nil "~[cero~;uno~;dos~:;mucho~]" 100)  ;; ~:; else-like
