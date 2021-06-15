(defpackage :model
  (:use :cl)
;  (:import-from :utils)
  (:export :register :registry)
  (:documentation "Model to support 7.8 Practical Session."))

(in-package :model)

;;; DATA

(defstruct (student (:print-function 
		     (lambda (s stream depth)
		       (declare (ignore depth))
		       (format stream "#<ID: ~A, Student: ~A, Module(s): ~A>"
			       (student-sid s)
			       (student-name s)
			       (student-modules s)))))
  (name (progn
	  (format t "Full Name? ")
	  (read)))
  (SID (incf *last-sid*))
  (modules nil))

(defparameter *students* nil
  "List of Students.")

(defparameter *last-sid* 0
  "Keeps track of last Student ID used.")

;;; FUNCTIONS

(defun register ()
  "Create a registry of new students and sets *STUDENTS*,
as a list of structures of type STUDENT."
  (error "TODO: Implementation"))

(defun registry (&optional (sid 0))
  "Returns the list of students, sorted alphabetically.
If the student ID is provided, return only that student information."
  (declare (ignore sid))
  (error "TODO: Implementation"))

