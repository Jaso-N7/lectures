(defpackage lecture-8
  (:use :cl)
  (:export :main))

(in-package :lecture-8)

;;; DATA

(defparameter *student-table*
  (make-hash-table :test #'equal)
  "Hash Table containing Students Name, ID and Modules.")

(defparameter *last-sid* 0
  "Keeps track of the last student ID.")

(defstruct (students (:print-function
		      (lambda (s stream depth)
			(declare (ignore depth))
			(format stream "#<SID: ~A, MODULES: ~A>"
			(students-sid s)
			(students-modules s)))))
  (sid (incf *last-sid*))
  (modules nil))

(defstruct (modules (:print-function
		     (lambda (m stream depth)
		       (declare (ignore depth))
		       (format stream "#<NAME: ~A, LECTURER: ~A, GRADE: ~A>"
			       (modules-name m)
			       (modules-lecturer m)
			       (modules-grade m)))))
  (name (progn
	  (format t "Module Name? ")
	  (read-line)))
  (lecturer (progn
	      (format t "Lecturer? ")
	      (read-line)))
  (grade nil))

;;; FUNCTIONS


(defun main ()
  "Starting point of the application."
  (format t "Ready to Rock!"))

(defun register-student ()
  "Store student records."
  (error "Not yet implemented"))

(defun find-student (name)
  "Find the record of a student with a given name NAME."
  (error "Not yet implemented"))

(defun delete-student (name)
  "Delete the record of the named student."
  (error "Not yet implemented."))

(defun un-enrolled-students ()
  "Return the SID of any student who hasn't attempted any modules at all."
  (error "Not yet implemented."))

(defun create-modules ()
  "Create the modules for students to enroll."
  (error "Not yet implemented."))

(defun enroll-students ()
  "Enroll students to modules."
  (error "Not yet implemented."))
