;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base; 10 -*-
;;;; ***************************************************************************
;;;; Name:       main.lisp
;;;; Purpose:    Regression tests and functional specs
;;;; Programmer: Jason Robinson
;;;;
;;;; This file may be broken up in the future to be more meaningful.
;;;;
;;;; ***************************************************************************

(in-package :lectures/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :lectures)' in your Lisp.

(defun random-list (length &key init-element
			        (min-number most-negative-double-float)
				(max-number most-positive-double-float))
  (typecase init-element
    (string (make-list length :initial-element init-element))
    (number (loop :repeat length
		  :collect
		  (+ min-number
		     (random (- max-number min-number)))))
    (otherwise (list 0 1 2 3 4 5 6 7 8 9
		     'a 'b 'c 'd 'e 'f 'g))))
  
				

(defun run-all-tests ()
  "Run all test cases for LECTURES package."
  (quickcheck
    (run-unit-tests)
    (run-quickchecks)))

(defun run-unit-tests ()
  "Unit Tests."
  (print "Lecture 10 Unit Tests:")
  (terpri)

  (named "SQRT-OF-SUM: Equality comparison"
    (is= (lec-10:sqrt-of-sum 42 700 8)
	 (lec-10:sqrt-of-sum 8 700 42)))
  
  (named "BETTER-EQUAL: Number Comparison"
    (is= T (lec-10::better-equal 14 14))
    (isnt= T (lec-10::better-equal 14 15)))
  (named "BETTER-EQUAL: Zero to one arguments of any type passed"
    (is= T (lec-10::better-equal))
    (is= T (lec-10::better-equal "aardvark"))
    (is= T (lec-10::better-equal 'a))
    (is= T (lec-10::better-equal '(a)))
    (is= T (lec-10::better-equal -13))))


(defun run-quickchecks ()
  "Property based tests"
  (print "Lecture 10 PBTs")
  (terpri)

    (named "BETTER-EQUAL String Comparison"
      (is= T
	   (lec-10::better-equal "aardvark" "aardvark" "aardvark" "aardvark"))
      (isnt= T
	     (lec-10::better-equal "aardvark" "aardvark" "goat" "aardvark"))
      (for-all ((strings (a-list #'a-string)))
	  (is lec-10::better-equal strings)))

    (named "BETTER-EQUAL: Number Comparison"
      (is= T (lec-10::better-equal 1 1 1 1 1 1 1 1 1 1 1))
      (for-all ((rand-nums (a-list an-integer)))
	  ;; Unit Tests already covers this case
	  (only-if (> (length rand-nums) 2)
		   (isnt= T
			  (apply #'lec-10::better-equal rand-nums)))))
  
  (named "BETTER-EQUAL (PBT): Mixed arguments."
    (isnt= T
	   (lec-10::better-equal #\I #\s #\n #\t))
    (isnt= T
	   (lec-10::better-equal 'a #\b "c" 1 2.0 pi))
      (for-all ((syms (a-list #'a-symbol))
		(nums (a-list an-integer))
		(strings (a-list #'a-string))
		(chars (a-list #'a-char)))
	(let ((mixed (append syms nums strings chars)))
	  (only-if (> (length mixed) 2)
		   (isnt= T (apply #'lec-10::better-equal mixed))))))
  
    (named "BETTER-EQUAL: Symbol Comparison"
      (is= T (lec-10::better-equal 'a 'a 'a))
      (for-all ((symbols (a-list #'a-symbol)))
	(only-if (> (length symbols) 2)
		 (isnt= T (apply #'lec-10::better-equal symbols))))))
		 


      

      


