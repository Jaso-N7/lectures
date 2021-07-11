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
    (char (make-list length :initial-element init-element))
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

  (named "BETTER-EQUAL: String Comparison" 
    (is= T
	 (lec-10::better-equal "aardvark" "aardvark" "aardvark" "aardvark"))
    (isnt= T
	   (lec-10::better-equal "aardvark" "aardvark" "goat" "aardvark")))
  (named "BETTER-EQUAL: Symbol Comparison"
    (is= T
	 (lec-10::better-equal 'a 'a 'a))
    (isnt= T
	   (lec-10::better-equal 'a 'a 'a 'b)))
  (named "BETTER-EQUAL: Mixed arguments."
    (isnt= T
	   (lec-10::better-equal #\I #\s #\n #\t))
    (isnt= T
	   (lec-10::better-equal 'a #\b "c" 1 2.0 pi)))
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
  (let ((list-length (random (1+ 42))))
    (for-all ((str #'a-string)
	      (str2 #'a-string))
      (let* ((lstr (random-list list-length :init-element str))
	     (lstr-rando (adjoin str2 lstr))) 
	(named "LEC-10::BETTER-EQUAL String Comparison"
	  (test (apply #'lec-10::better-equal lstr))
	  (is lec-10::better-equal lstr-rando))))

    (let ((list-nums '(1 1 1 1 1 1 1 1 1 1 1)))
      (for-all ((rand-nums (a-list an-integer)))
	(named "LEC-10 BETTER-EQUAL: Number Comparison"
	  (is lec-10::better-equal list-nums)
	  ;; Unit Tests already covers this case
	  (only-if (>= (length rand-nums) 2)
		   (isnt= T
			  (apply #'lec-10::better-equal rand-nums))))))))
;	  (test (apply #'lec-10::better-equal rand-nums)))))))
      

      


