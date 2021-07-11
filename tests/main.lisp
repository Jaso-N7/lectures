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
  (terpri)
  (princ "Lecture 10 Unit Tests:")
  (terpri)
  (is= T
       (lec-10::better-equal "aardvark" "aardvark" "aardvark" "aardvark"))
  (is= NIL
       (lec-10::better-equal "aardvark" "aardvark" "goat" "aardvark")))

(defun run-quickchecks ()
  "Property based tests"
  (format t "~&Lecture 10 PBTs:~%"))


