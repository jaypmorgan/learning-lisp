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

;; 6.11 Write a function MAKE-PALINDROME that makes a palindrome out
;; of a list, for example, given (YOU AND ME) as input it should
;; return (YOU AND ME ME AND YOU).

(defun make-palindrome (lst)
  (append lst (reverse lst)))

(make-palindrome '(you and me))

;; 6.24 Sets are said to be equal if they contain exactly the same
;; elements. Order does not matter in a set, so the sets (RED BLUE
;; GREEN) and (GREEN BLUE RED) are considered equal. However, the
;; equal predicate does not consider them equal, because it treats
;; them as lists, not as sets. Write a SEQ-EQUAL predicate that
;; returns T if two things are equal as sets. {Hint: If two sets are
;; equal, then each is a subset of the other}.

(defun set-equal (lst1 lst2)
  (and (subsetp lst1 lst2) (subsetp lst2 lst1)))

(set-equal '(1 2 3) '(3 2 1))
(set-equal '(1 2) '(3 2 1))

;; 6.25 A set X is a proper subset of a set Y if X is a subset of Y
;; but not equal to Y. Thus, (A C) is a proper subset of (C A B). (A B
;; C) is a subset of (C A B), but not a proper subset of it. Write the
;; PROPER-SUBSETP predicate, which returns T if its first input is a
;; proper subset of its second input.

(defun proper-subsetp (lst1 lst2)
  (and (subsetp lst1 lst2) (not (set-equal lst1 lst2))))

(proper-subsetp '(A C) '(C A B))
(proper-subsetp '(A C B) '(C A B))

;; 6.21 If set x is a subset of set y, then subtracting y from x
;; should leave the empty set. Write MY-SUBSETP, a version of SUBSETP
;; predicate that returns T if its first input is a subset of its
;; second input.

(defun my-subsetp (lst1 lst2)
  (eq nil (set-difference lst1 lst2)))

(my-subsetp '(A B) '(C A B))
(my-subsetp '(A E) '(C A B))

;; 6.26 We are going to write a program that compares the description
;; of two objects and tells how many features they have in common. The
;; descriptions will be represented as a list of features, with the
;; symbol -VS- separating the first object from the second. Thus, when
;; given a list like:
;;
;; (large red shiny cube -vs- small shiny red four-sided pyramid)
;;
;; The program will respond with (2 COMMON FEATURES). We will compose
;; this program from several small functions that you will write and
;; test one at a time.
;;
;; a. Write a function RIGHT-SIDE that returns all the features to the
;; right of the -VS- symbol. RIGHT-SIDE of the list shown above should
;; return (SMALL RED SHINY RED FOUR-SIDED PYRAMID). Hint: remember
;; that the member function returns the entire sublist starting with
;; the item for which you are searching. Test your function to make
;; sure it works correctly.

(defparameter test-features '(large red shiny cube -vs- small shiny red four-sided pyramid))

(defun right-side (lst)
  (cdr (member '-vs- lst)))

(right-side test-features)

;; b. Write a function LEFT-SIDE that returns all the features to the
;; left of -VS-. You can't use the MEMBER trick directly for this one,
;; but you can use it if you do something to the list first.

(defun left-side (lst)
  (reverse (cdr (member '-vs- (reverse lst)))))

(left-side test-features)

;; c. Write a function COUNT-COMMON that returns the number of
;; features the left and right sides of the input have in common.

(defun count-common (lst)
  (let ((left (left-side lst))
	(right (right-side lst)))
    (length (intersection left right))))

(count-common test-features)

;; d. Write the main function, COMPARE, that takes a list of features
;; describing two objects, with a -VS- between them, and reports the
;; number of features they have in common. COMPARE should return a
;; list of form (n COMMON FEATURES).

(defun compare (lst)
  `(,(count-common lst) COMMON FEATURES))

;; e. Try the expression (compare '(small red metal cube -vs- red
;; plastic small cube)) You should get (3 COMMON FEATURES) as the
;; result.

(compare '(small red meta cube -vs- red plastic small cube))
