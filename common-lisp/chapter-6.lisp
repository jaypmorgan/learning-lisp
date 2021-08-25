;; 6.6 Use the LAST function to write a function called LAST-ELEMENT
;; that returns the last element of a list instead of the last cons
;; cell. Write another version of LAST-ELEMENT usng REVERSE instead of
;; LAST. Write another version using NTH and LENGTH.

(defun last-element (lst)
  (car (last lst)))

(last-element '(1 2 3 4))

(defun last-element (lst)
  (car (reverse lst)))

(last-element '(1 2 3 4))

(defun last-element (lst)
  (nth (1- (length lst)) lst))

(last-element '(1 2 3 4))

;; 6.7 Use REVERSE to write a NEXT-TO-LAST function that returns the
;; next-to-last element of a list. Write another version using NTH.

(defun next-to-last (lst)
  (cadr (reverse lst)))

(next-to-last '(1 2 3 4))

(defun next-to-last (lst)
  (nth (- (length lst) 2) lst))

(next-to-last '(1 2 3 4))

;; 6.8 Write a function MY-BUTLAST that returns a list with the last
;; element removed. (MY-BUTLAST '(ROSES ARE RED)) should return the
;; list (ROSES ARE). (MY-BUTLAST '(G A G A)) should return (G A G).

(defun my-butlast (lst)
  (reverse (cdr (reverse lst))))

(my-butlast '(ROSES ARE RED))
(my-butlast '(G A G A))

;; 6.10 A palindrome is a sequence that reads the same forwards and
;; backwards. The list (A B C D C B A) is a palindrome; (A B C A B C)
;; is not. Write a function PALINDROMEP that returns T if its input is
;; a palindrome.

(defun palindromp (lst)
  (equal lst (reverse lst)))

(palindromp '(A B C D C B A))
(palindromp '(A B C A B C))
