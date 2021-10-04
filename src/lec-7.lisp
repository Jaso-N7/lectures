(defpackage :lectures-7
  (:use :cl)
  (:import-from :model :register :registry :new-module)
  (:export :main-menu)
  (:documentation "Main package to be loaded."))

(in-package :lectures-7)

;; DATA STRUCTURES
;; !!! - TODO: Move this section to a CONTROLLER package.

(defstruct menu-item display shortcut actions)

(defparameter *menues*
  (vector
   (make-menu-item
    :display "(A)dd Student~%"
    :shortcut "a"
    :actions #'(lambda ()
		 (register)
		 (main-menu)))
   (make-menu-item
    :display "(B)rowse all students~%"
    :shortcut "b"
    :actions #'(lambda ()
		 (registry)
		 (main-menu)))
   (make-menu-item
    :display "(C)all by student ID~%"
    :shortcut "c"
    :actions #'(lambda ()
		 (format t "~&Student ID: ")
		 (let ((id (parse-integer (read-line))))
		   (registry id)
		   (main-menu))))
   (make-menu-item
    :display "(D)efine Modules~%"
    :shortcut "d"
    :actions #'(lambda ()
		 (new-module)
		 (main-menu)))
   (make-menu-item
    :display "(E)nroll students to Modules~%"
    :shortcut "e"
    :actions #'(lambda ()
		 (format t "~&Not yet implemented.~%")
		 (main-menu)))
   (make-menu-item
    :display "(Q)uit~%"
    :shortcut "q"
    :actions #'(lambda ()
		 (format t "~&Goodbye")))
   (make-menu-item
    :display "(F)ind Student by Module"
    :shortcut "f"
    :actions
    #'(lambda ()
	(format t "~&Unknown input, kindly choose (a/b/c/d/e or q)?~%")
	(main-menu)))))

(defun generate-menu (menu size)
  "Given the MENU and its SIZE, displays the menu items found in MENU."
  (dotimes (m size)
    (format t (menu-item-display
	       (svref menu m)))))

(defun event-handler (shortcut menu)
  "Given the SHORTCUT key entered by the user, call the
appropriate event-handler as defined in the MENU."
  (find-if #'(lambda (m)
	       (string= shortcut
			(menu-item-shortcut m)))
	   menu))

(defun on-select (fn &rest args)
  (funcall (menu-item-actions 
	    (apply fn args))))

(defun main-menu ()
  (format t "~&Welcome to the Student Directory:~%")
  (generate-menu *menues* (length *menues*))
  (format t "~&Your input: ")
  (on-select #'event-handler (read-line) *menues*))

	  
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
