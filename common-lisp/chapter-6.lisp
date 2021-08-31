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

;; 6.28 Set the global variable PRODUCE to this list:

(defparameter produce
  '((apple	. fruit)
    (celery	. veggie)
    (banana	. fruit)
    (lettuce	. veggie)))

;; Now write down the results of the following expressions:

(assoc 'banana produce) ;; => (BANANA . FRUIT)
(rassoc 'fruit produce) ;; => (APPLE . FRUIT)
(assoc 'lettuce produce) ;; => (LETTUCE . VEGGIE)
(rassoc 'veggie produce) ;; => (CELERY . VEGGIE)

;; 6.30 Make a table called books of five books and their authors. The
;; first entry might be (WAR-AND-PIECE LEO-TOLSTOY).

(defparameter books
  '((war-and-piece leo-tolstoy)
    (the-catcher-in-the-rye j-d-salinger)
    (frankenstein mary-shelly)
    (hitchhikers-guide-to-the-galaxy douglas-adams)
    (the-waves virginia-wolf)))

;; 6.31 Write the function WHO-WROTE that takes the name of a book as
;; input and returns the book's author.

(defun who-wrote (book-name)
  (second (assoc book-name books)))

(who-wrote 'war-and-piece) ;; => LEO-TOLSTOY
(who-wrote 'frankenstein) ;; => MARY-SHELLY
(who-wrote 'common-lisp) ;; => NIL

;; Mini-keyboard exercise

;; 6.35 In this problem we will simulate the
;; behaviour of a very simple-minded creature, /Nerdus Americanis/
;; (also known as /Computerus Hackerus/). This creature has only five
;; states: Sleeping, Eating, Waiting-for-a-computer, Programming, and
;; Debugging. Its behaviour is cyclic: After it sleeps it always eats,
;; after it eats it always waits for a computer, and so on, until
;; after debugging it goes back to sleep for a while.
;;
;; a. What type of data structure would be useful for representing the
;; connection between a state and its successor? Write such a data
;; structure for the five-state cycle given above, and store it in a
;; global variable called NERD-STATES.

;; a simple table structure should work fine, though if it wanted
;; something more complex, we could set the cdr of the end of a list
;; of states to the start of the list, making a cycle.
(defparameter nerd-states
  '((sleeping eating)
    (eating waiting-for-a-computer)
    (waiting-for-a-computer programming)
    (programming debugging)
    (debugging sleeping)))

;; b. Write a function NERDUS that takes the name of a state as input
;; and uses the data structure you designed to determine the next
;; state the creature will be in. (NERDUS 'SLEEPING) should return
;; EATING, for example. (NERDUS 'DEBUGGING) should return SLEEPING.

(defun nerdus (state)
  (cadr (assoc state nerd-states)))

(nerdus 'sleeping)
(nerdus 'debugging)

;; c. What is the result of (NERDUS 'PLAYING-GUITAR)?

(nerdus 'playing-guitar) ;; => NIL

;; d. When /Nerdus Americanis/ ingests too many stimulants (caffeine
;; overdose), it stops sleeping. After finishing Debugging, it
;; immediately goes on to state Eating. Write a function
;; SLEEPLESS-NERD that works just like NERDUS except it never
;; sleeps. Your function should refer to the global variable
;; NERD-STATES, as NERDUS does.

(defun sleepless-nerd (state)
  (let ((next-state (nerdus state)))
    (if (eq next-state 'sleeping)
	(nerdus 'sleeping)
	next-state)))

(sleepless-nerd 'sleeping) ;; => EATING
(sleepless-nerd 'eating) ;; => WAITING-FOR-A-COMPUTER

;; e. Exposing /Nerdus Americanis/ to extreme amounts of chemical
;; stimulants produces pathological behaviour. Instead of an orderly
;; advance to its next state, the create advances two states. For
;; example, it goes from Eating directly to Programming, and from
;; there to Sleeping. Write a function NERD-ON-CAFFEINE that exhibits
;; this unusual pathology. Your function should use the same table as
;; NERDUS.

(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)))

(nerd-on-caffeine 'eating)
(nerd-on-caffeine 'programming)

;; Review exercises
;;
;; 6.36 Write a function to swap the first and last elements of any
;; list. (SWAP-FIRST-LAST '(YOU CANT BUY LOVE)) should return (LOVE
;; CANT BUY YOU).

(defun swap-first-last (lst)
  (let ((first-el (car lst))
	(last-el (car (last lst))))
    `(,last-el ,@(remove last-el (cdr lst)) ,first-el)))

(swap-first-last '(you cant buy love))

;; 6.37 ROTATE-LEFT and ROTATE-RIGHT are functions that rotate the
;; element of a list. (ROTATE-LEFT '(A B C D E)) returns (B C D E A),
;; whereas ROTATE-RIGHT returns (E A B C D). Write these functions.

(defun rotate-right (lst)
  (append (cdr lst) (list (car lst))))

(defun rotate-left (lst)
  (append (last lst) (remove (car (last lst)) lst)))

(rotate-right '(a b c d e))
(rotate-left '(a b c d e))

;; 6.38 Give an example of two sets X and Y such that (SET-DIFFERENCE
;; X Y) equals (SET-DIFFERENCE Y X). Also give an example in which the
;; set differences are not equal.

(eq (set-difference nil nil) (set-difference nil nil))
(eq (set-difference '(1) nil) (set-difference nil '(1)))

;; 6.40 Show how to transform the list (A B C D) into a table so that
;; the ASSOC function using the table gives the same results as MEMBER
;; using the list.

(defparameter table-transform
  '((A . (B C D))
    (B . (C D))
    (C . (D))
    (D . nil)))

(member 'A '(A B C D))
(member 'B '(A B C D))
(member 'C '(A B C D))
(member 'D '(A B C D))
(assoc 'A table-transform)
(assoc 'B table-transform)
(assoc 'C table-transform)
(assoc 'D table-transform)
