;; #+title: Exponentation
;; #+author: Jay Morgan
;; #+date: <2023-01-22 Sun>
;; #+property: header-args:scheme :session :eval never-export :tangle exponentiation.scm :comments both

;; One way to compute the exponential of a given number $b$ to the power $n$ is a
;; recursive process:

;; \[
;; b^n = b \cdot b^{n-1}
;; \]
;; \[
;; b^0 = 1
;; \]

;; We can write this in scheme:


;; [[file:exponentiation.org::+begin_src scheme][No heading:1]]
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
;; No heading:1 ends here



;; #+RESULTS:
;; : #<void>

;; We can see that it is a linear recursive process by tracing the function and
;; visualising the function call stack:


;; [[file:exponentiation.org::+begin_src scheme :results value :exports both][No heading:2]]
(expt 2 5)
;; No heading:2 ends here



;; #+begin_example
;; |(expt 2 5)
;; | (expt 2 4)
;; | |(expt 2 3)
;; | | (expt 2 2)
;; | | |(expt 2 1)
;; | | | (expt 2 0)
;; | | | 1
;; | | |2
;; | | 4
;; | |8
;; | 16
;; |32
;; 32
;; #+end_example

;; This requires $\Theta(n)$ steps and $\Theta(n)$ space. This can be reformulated as a
;; linear iterative process.


;; [[file:exponentiation.org::+begin_src scheme][No heading:3]]
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
	product
	(expt-iter (- counter 1) (* b product))))
  (expt-iter n 1.0))
;; No heading:3 ends here



;; #+RESULTS:
;; : #<void>

;; This still requires $\Theta(n)$ calls (to the internal =expt-iter= function, but only
;; requires $\Theta(1)$ space due to the running product variable and the tail-call
;; optimised recursion.

;; The exponential can be computed in a smaller number of steps than $\Theta(n)$ using
;; the following definition:

;; \[
;; b^n = (b^{n/2})^2  \, \text{if } n \text{ is even}
;; \]
;; \[
;; b^n = b \cdot b^{n-1} \, \text{if } n \text{ is odd}
;; \]

;; And the scheme implementation:


;; [[file:exponentiation.org::+begin_src scheme][No heading:4]]
(define (expt b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
	((even? n) (square (expt b (/ n 2))))
	(else (* b (expt b (- n 1))))))
;; No heading:4 ends here
