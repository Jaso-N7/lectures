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

(defparameter *module-table*
  (make-hash-table :test #'equal)
  "Keep track of modules.")


(defstruct (students (:print-function
		      (lambda (s stream depth)
			(declare (ignore depth))
			(format stream "#<SID: ~A, NAME: ~A, MODULES: ~A>"
				(students-sid s)
				(students-name s)
				(students-modules s)))))
  (name nil)
  (sid (incf *last-sid*))
  (modules nil))

(defstruct (modules (:print-function
		     (lambda (m stream depth)
		       (declare (ignore depth))
		       (format stream "#<MID: ~A, NAME: ~A, LECTURER: ~A, GRADE: ~A>"
			       (modules-mid m)
			       (modules-name m)
			       (modules-lecturer m)
			       (modules-grade m)))))
  (name (progn
	  (format t "Module Name? ")
	  (read-line)))
  (mid nil)
  (lecturer (progn
	      (format t "Lecturer? ")
	      (read-line)))
  (grade nil))

;;; FUNCTIONS


(defun main ()
  "Starting point of the application."
  (format t "Ready to Rock!"))

(defun register-students ()
  "Store student records."
  (format t "~&Student Name? ")
  (let* ((name (read-line))
	 (proname (string-capitalize (string-downcase name))))
    (setf (gethash proname *student-table*)
	  (make-students :name proname))
    (format t "Add more (y/n)? ")
    (let ((yn (read-line)))
      (if (char= (char yn 0) #\n)
	  (view-students)
	  (register-student)))))

(defun view-students ()
  "View all students registered so far."
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (format t "~&~A" v))
	   *student-table*))

(defun find-student (name)
  "Find the record of a student with a given name NAME."
  (let* ((proname (string-capitalize (string-downcase name)))
	 (info (gethash proname *student-table*)))
    (if info
	(format t "~&~A~%" info)
	(format t "~&No student record found."))))

(defun delete-student (name)
  "Delete the record of the named student."
  (let* ((proname (string-capitalize (string-downcase name)))
	 (info (gethash proname *student-table*)))
    (cond (info
	   (remhash proname *student-table*)
	   (format t "~&~A successfully deleted." proname))
	  (t
	   (format t "~&No record found, confirm spelling.~%Nothing deleted.")))))

(defun un-enrolled-students ()
  "Return the SID of any student who hasn't attempted any modules at all."
  (error "Not yet implemented."))

(defun create-modules ()
  "Create the modules for students to enroll."
  (format t "~&Module ID? ")
  (let ((mid (read-line)))
    (setf (gethash mid *module-table*)
	  (make-modules :mid mid))
    (format t "~&Add more (y/n)? ")
    (let ((yn (read-line)))
      (if (string= yn "n")
	  (view-modules)
	  (create-modules)))))

(defun view-modules ()
  "Display all modules in the *MODULE-TABLE*"
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (format t "~&~A" v))
	   *module-table*))

(defun enroll-students ()
  "Enroll students to modules."
  (error "Not yet implemented."))
