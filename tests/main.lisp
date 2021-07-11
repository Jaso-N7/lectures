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

(defun run-all-tests ()
  "Run all test cases for LECTURES package."
  (run-unit-tests)
  (run-quickchecks))

(defun run-unit-tests ()
  "Unit Tests."
  (print "Lecture 10 Unit Tests:")
  (terpri)
  (quickcheck
    (named "String Comparison" 
      (is= T
	   (lec-10::better-equal "aardvark" "aardvark" "aardvark" "aardvark"))
      (isnt= T
	     (lec-10::better-equal "aardvark" "aardvark" "goat" "aardvark")))
    (named "Number Comparison"
      (is= T
	   (lec-10::better-equal 1 1 1 1 1 1 1 1 1 1 ))
      (isnt= T
	     (lec-10::better-equal 1 1 1 5 5 5 42 42 5 5)))
    (named "Symbol Comparison"
      (is= T
	   (lec-10::better-equal 'a 'a 'a))
      (isnt= T
	   (lec-10::better-equal 'a 'a 'a 'b)))
    (named "Mixed arguments."
      (isnt= T
	     (lec-10::better-equal #\I #\s #\n #\t))
      (isnt= T
	     (lec-10::better-equal 'a #\b "c" 1 2.0 pi)))
    (named "Amount of arguments passed"
      (is= T (lec-10::better-equal))
      (is= T (lec-10::better-equal "aardvark")))))


(defun run-quickchecks ()
  "Property based tests"
  (quickcheck
    (named "Lecture 10 PBTs"
      (is= 1 1))))


