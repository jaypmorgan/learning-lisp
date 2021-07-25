;; Looping with the loop command
(loop for i below 5 sum i) ;; 0 + 1 + 2 + 4 = 10

;; loop tricks
;; counting from a starting point to an ending point
(loop for i from 5 to 10 sum i)

;; iterating through values in a list
(loop for i in '(100 20 3) sum i)

;; doing stuff in a loop
(loop for i below 5 do (print i))

;; doing stuff under certain conditions
(loop for i below 10 when (oddp i) sum i)

;; break out of the loop early
(loop for i from 0 do (print i) when (= i 5) return 'falafel)

;; collecting a list of values
(loop for i in '(2 3 4 5 6) collect (* i i))

;; using multiple for clauses
(loop for x below 10
      for y below 10
      collect (+ x y))

;; the multiple clause loops ends when one of the lists runs out of
;; values, it is not a nested for loop like in other languages.
;; cartesian product of loops:
(loop for x below 10
      collect (loop for y below 10
		    collect (+ x y)))

(loop for i from 0
      for day in '(monday tuesday wednesday thursday friday saturday sunday)
      collect (cons i day))

