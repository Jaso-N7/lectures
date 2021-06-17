(defpackage :model
  (:use :cl)
  (:export :register :registry)
  (:documentation "Model to support 7.8 Practical Session."))

(in-package :model)

;;; DATA

(defstruct (student (:print-function 
		     (lambda (s stream depth)
		       (declare (ignore depth))
		       (format stream "#<ID: ~A, STUDENT: ~A, MODULE(S): ~A>"
			       (student-sid s)
			       (student-name s)
			       (student-modules s)))))
  (name (progn
	  (format t "Full Name? ")
	  (read-line)))
  (SID (incf *last-sid*))
  (modules nil)) ; may make this an array containing module ID and Grade.

(defstruct (node (:print-function (lambda (n s d)
				    (declare (ignore d))
				    (format s "#<~A>" (node-elt n)))))
  elt
  (l nil)
  (r nil))

(defstruct (modules (:print-function (lambda (m stream depth)
				       (declare (ignore depth))
				       (format stream "#<MID: ~A, MODULE: ~A, LECTURER: ~A>"
					       (modules-mid m)
					       (modules-name m)
					       (modules-lecturer m)))))
  (name (progn
	  (format t "Module Name? ")
	  (read-line)))
  (mid (incf *last-mid*))
  ;; We are assuming one(1) lecturer / module
  (lecturer (progn
	      (format t "Module Lecturer? ")
	      (read-line))))

(defparameter *last-sid* 0
  "Keeps track of last Student ID used.")

(defparameter *last-mid* 0
  "Keeps track of last Module ID used.")

(defparameter *student-bst* nil
  "Binary Search Tree used for storing students.")

(defparameter *modules* nil
  "Database for Modules - not sure if this should be an array as yet.")

;;; FUNCTIONS

(defun register ()
  "Create a registry of new students and sets *STUDENTS*,
as a list of structures of type STUDENT."
  (let ((students ())
	(student (make-student)))
    (format t "Continue (y/n)? ")
    (let ((yn (read-line)))
      (cond ((string= "y" yn)
	     (push student students)
	     (register))
	    ((string= "n" yn)
	     (dolist (s students)
	       (setf *student-bst* (bst-insert s *student-bst* #'string<)))
	     (display-students))
	    (t  (format t "~&Unknown input, returning to main menu."))))))

(defun registry (&optional (sid 0))
  "Returns the list of students, sorted alphabetically.
If the student ID is provided, return only that student information."
  (if (/= sid 0)
      (find-student sid)
      (let ((resp nil))
	(format t "~&View (a)ll students, search by (I)D or go (b)ack (a/i/b)? ")
	(setf resp (read-line))
	  (cond ((string-equal "a" resp)
		 (display-students)
		 (registry))
		((string-equal "i" resp)
		 (format t "Student ID? ")
		 (let ((id (read)))
		   (registry id)))
		(t  nil)))))



(defun display-students ()
  (bst-traverse #'print
		*student-bst*))
    
(defun find-student (sid)
  "Retrieve student information with a given SID."
  (bst-traverse #'(lambda (s)
		    (when (= sid (student-sid s))
		      (print s)))
		*student-bst*))

	     
(defun bst-insert (obj bst <)
  "Insert an object OBJ into the provided Binary Search Tree BST;
Otherwise create a new BST."
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
	(cond ((eql obj elt) 
	       bst)
	      ((funcall < (student-name obj) (student-name elt))
	       (make-node :elt elt
			  :l   (bst-insert obj (node-l bst) <)
			  :r   (node-r bst)))
	      (t  (make-node :elt elt
			     :l (node-l bst)
			     :r (bst-insert obj (node-r bst) <)))))))

(defun bst-find (obj bst <)
  "Find Object OBJ in the Binary Search Tree BST.
Return NIL if not found."
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
	(cond ((eql obj elt)
	       bst)
	      ((funcall < (student-name obj) (student-name elt))
	       (bst-find obj (node-l bst) <))
	      (t  (bst-find obj (node-r bst) <))))))

(defun bst-traverse (fn bst)
  "Binary Search Tree traversal."
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))
