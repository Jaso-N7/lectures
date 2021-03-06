(defpackage lectures-8
  (:use :cl)
  (:export :main))

(in-package :lectures-8)

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
  (modules nil)) ; modules will be stored as hash tables

(defstruct (modules (:print-function
		     (lambda (m stream depth)
		       (declare (ignore depth))
		       (format stream "#<MID: ~A, NAME: ~A, LECTURER: ~A>"
			       (modules-mid m)
			       (modules-name m)
			       (modules-lecturer m)))))
  (name (progn
	  (format t "Module Name? ")
	  (read-line)))
  (mid nil)
  (lecturer (progn
	      (format t "Lecturer? ")
	      (read-line))))


(defconstant days-of-week
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defconstant months #("Jan" "Feb" "Mar" "Apr"
		      "May" "Jun" "Jul" "Aug"
		       "Sep" "Oct" "Nov" "Dec"))

;;; UTILITIES

(defun normalize-name (name)
  (string-capitalize (string-downcase name)))


(defun date-1 ()
  "Emulates UNIX date(1) command. Return example: Fri Sep 15
14:12:41 BST 2000"
  (multiple-value-bind (s m h dt mth y d dst tz) (get-decoded-time)
    (declare (ignore dst))
    (format nil "~A ~A ~A ~A:~2,,,'0@A:~2,,,'0@A ~A ~A"
	    (svref days-of-week d)
	    (svref months mth)
	    dt h m s tz y)))

;;; FUNCTIONS


(defun main ()
  "Starting point of the application."
  (format t "~&TODO!~%")
  (format t "Revise ENROLL-STUDENTS to save modules as a HASH-TABLE in lieu of a LIST.~%")
  (format t "Revise student information to reveal grades.~%")
  (format t "Revise all functions that handle setting student grades.~%")
  (format t "Complete functions yet to be implemented."))

(defun register-students ()
  "Store student records."
  (format t "~&Student Name? ")
  (let ((proname (normalize-name (read-line))))
    (setf (gethash proname *student-table*)
	  (make-students :name proname))
    (format t "Add more (y/n)? ")
    (let ((yn (read-line)))
      (if (char= (char yn 0) #\n)
	  (view-students)
	  (register-students)))))

(defun view-students ()
  "View all students registered so far."
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (format t "~&~A" v))
	   *student-table*))

(defun find-student (name)
  "Find the record of a student with a given name NAME."
  (let* ((proname (normalize-name name))
	 (info (gethash proname *student-table*)))
    (students-p
     (if info
	 (print info)
	 (print "No student record found.")))))

(defun delete-student (name)
  "Delete the record of the named student."
  (let* ((proname (normalize-name name))
	 (info (gethash proname *student-table*)))
    (cond (info
	   (remhash proname *student-table*)
	   (format t "~&~A successfully deleted." proname))
	  (t
	   (format t "~&No record found, confirm spelling.~%Nothing deleted.")))))

(defun un-enrolled-students ()
  "Return the list of SIDs of any student who hasn't attempted any modules at all."
  (let ((unenrolled ()))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (when (null (students-modules value))
		   (push (students-sid value) unenrolled)))
	     *student-table*)
    unenrolled))

(defun un-enrolled-student ()
  "Return the SID of any student who hasn't attempted any modules at all."
  (maphash #'(lambda (key value)
	       (declare (ignore key))
	       (when (null (students-modules value))
		 (return-from un-enrolled-student
		   (students-sid value))))
	   *student-table*))


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
  (labels ((enroll (mid)
	     (format t "Add Student by Full Name: ")
	     (let* ((proname (normalize-name (read-line)))
		    (student (gethash proname *student-table*)))
	       (push `(,mid 0) (students-modules student))
	       (format t "~&Add more (y/n)? ")
	       (let ((resp (read-line)))
		 (if (string= resp "n")
		     (view-students)
		     (enroll mid))))))
    (format t "~&Module ID? ")
    (enroll (read-line))))

(defun record-marks ()
  "Recording marks for modules which students have already attempted.
Returns a table of students with their updated marks."
  (maphash #'(lambda (k v)
	       (let ((modules (students-modules v)))
		 (unless (null modules)
		   (record-mark k modules))))
	   *student-table*)
  (view-students))
	       
(defun record-mark (student modules)
  "Go through the list of enrolled modules and update the marks."
  (unless (null modules)
    (let ((module (caar modules)))
      (format t "~&Update ~A's mark for ~A (y/n)? "
	      student module)
      (let ((yn (read-line)))
	(when (string= yn "y")
	  (format t "Set mark [~A]: " (cadar modules))
	  (setf (cadar modules)
		(parse-integer (read-line)))))))
  (record-mark student (cdr modules)))
	  
		


(defun total-marks ()
  "Use REDUCE to find the total number of marks over all modules attempted by each student and hence the total marks obtained by the student cohort"
  (error "Not yet implemented."))

(defun avg-marks ()
  "count how many modules were attempted by the cohort, and hence give the average mark"
  (error "Not yet implemented."))


#|
!!! TODO: As before, write functions to name the three students who have the highest marks, or to spot which lecturer fails most of their students.


|#
