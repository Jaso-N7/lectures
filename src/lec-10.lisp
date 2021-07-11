(defpackage lectures-10
  (:use :cl)
  (:nicknames "LEC-10")
  (:export :sqrt-of-sum
   :sqrt-sum-sqr
   :better-equal)
  (:documentation "Suggested activity and exercises for Lecture 10."))

(in-package :lectures-10)


;;; 10.8 Practical session / Suggested activity

(defun sqrt-of-sum (&rest args)
  "Takes any number of arguments, and computes the square root of the sum of 
their squares."
  (let ((sum 0))
    (dolist (arg args (sqrt sum))
      (incf sum (* arg arg)))))

(defun sqrt-sum-sqr (&rest args)
  "Same as `SQRT-OF-SUM'"
  (sqrt (reduce #'+ (mapcar #'(lambda (a)
				(* a a))
			    args))))
			    
(defun better-equal (&rest args)
  (let ((head (car args))
	(tail (cdr args)))
    (dolist (a tail T)
      (unless (equal a head)
	(return nil)))))
