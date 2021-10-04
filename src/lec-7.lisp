(defpackage :lectures-7
  (:use :cl)
  (:import-from :model :register :registry :new-module)
  (:export :main-menu)
  (:documentation "Main package to be loaded."))

(in-package :lectures-7)

;; DATA STRUCTURES

(defstruct menu-item actions)

(defparameter menu-display
  (make-menu-item
   :actions #'(lambda ()
		(format t "~&Welcome to the Student Directory:~%")
		(format t "(A)dd Student~%")
		(format t "(B)rowse all students~%")
		(format t "(C)all by student ID~%")
		(format t "(D)efine Modules~%")
		(format t "(E)nroll students to Modules~%")
		(format t "(Q)uit~%")
		(format t "Your input: "))))

(defparameter add-students
  (make-menu-item :actions #'(lambda ()
			       (register)
			       (main-menu))))

(defparameter lookup-students
  (make-menu-item :actions #'(lambda ()
			       (registry)
			       (main-menu))))

(defparameter lookup-student
  (make-menu-item :actions #'(lambda ()
			       (format t "~&Student ID: ")
			       (let ((id (parse-integer (read-line))))
				 (registry id)
				 (main-menu)))))

(defparameter add-modules
  (make-menu-item :actions #'(lambda ()
			       (new-module)
			       (main-menu))))

(defparameter student-modules
  (make-menu-item :actions #'(lambda ()
			       (format t "~&Not yet implemented.~%")
			       (main-menu))))

(defparameter cleanup
  (make-menu-item :actions #'(lambda ()
			       (format t "~&Goodbye"))))

(defparameter unknown
  (make-menu-item
   :actions
   #'(lambda ()
       (format t "~&Unknown input, kindly choose (a/b/c/d/e or q)?~%")
       (main-menu))))

;; FUNCTIONS

(defun on-select (menu)
  (funcall (menu-item-actions menu)))
		    
(defun main-menu ()
  (on-select menu-display)
  (let ((in (read-line)))
    (cond ((string-equal "a" in)
	   (on-select add-students))
	  ((string-equal "b" in)
	   (on-select lookup-students))
	  ((string-equal "c" in)
	   (on-select lookup-student))
	  ((string-equal "d" in)
	   (on-select add-modules))
	  ((string-equal "e" in)
	   (on-select student-modules)
	  ((string-equal "q" in)
	   (on-select cleanup))
	  (t  (on-select unknown))))))
	  
(defun enroll-students ()
  "Enroll students to modules."
  (format t "~&Enroll by ID, Un-enrolled only, or cancel?~%(i/u/x): ")
  (let ((choice (read-line)))
    (cond ((char= #\i (char choice 0))
	   (error "Enroll by ID not yet implemented."))
	  ((char= #\u (char choice 0))
	   (error "Un-enrolled only not yet implemented."))
	  ((char= #\x (char choice 0))
	   (format t "~&Returning to main screen."))
	  (t  (format t "~&Unknown choice")
	      (enroll-students)))))
