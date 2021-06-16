(defpackage :model
  (:use :cl)
  (:import-from :utils :bst-insert :bst-find :bst-traverse)
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

(defparameter *student-bst* nil
  "Binary Search Tree used for storing students.")

;;; FUNCTIONS

(defun register ()
  "Create a registry of new students and sets *STUDENTS*,
as a list of structures of type STUDENT."
  (let ((pupil (make-student)))
    (format t "Continue (y/n)? ")
    (let ((yn (read-line)))
      (cond ((string= "y" yn)
	     (push pupil *students*)
	     (register))
	    ((string= "n" yn)
	     (dolist (s *students*)
	       (setf *student-bst* (bst-insert s *student-bst* #'string<)))
	     (display-students))
	    (t  (format t "~&Unknown input, returning to main menu."))))))

(defun registry (&optional (sid 0))
  "Returns the list of students, sorted alphabetically.
If the student ID is provided, return only that student information."
  (if (/= sid 0)
      (find-student sid)
      (let ((resp nil)
	    (id 0))
	(format t "View (a)ll students, search by (I)D or go (b)ack (a/i/b)? ")
	(setf resp (read-line))
	  (cond ((string-equal "a" resp)
		 (display-students)
		 (registry))
		((string-equal "i" resp)
		 (format t "Student ID? ")
		 (setf id (parse-integer (read-line)))
		 (registry id))
		(t  nil)))))

(defun display-students ()
  (bst-traverse #'(lambda (s)
		    (format t "~A~%" s))
		*student-bst*))
    
(defun find-student (sid)
  (bst-find sid *student-bst* #'<))
	     
