(defpackage :utils
  (:use :cl)
  (:documentation "Utility Functions."))

(in-package :utils)

;;; ANSI CL by Paul Graham - Chapter 6

;; 2
(defun bin-search (obj vec
		   &key
		     (key #'finder)
		     (test #'(lambda ()
			       (not (zerop (length vec)))))
		     (start 0)
		     (end (1- (length vec))))
    (and test
	 (funcall key obj vec start end)))

(defun finder (obj vec start end)
  (let ((range (- end start)))
    (if (zerop range)
      (when (eql obj (aref vec start))
	  obj)
      (let ((mid (+ start (round (/ range 2)))))
	(let ((obj2 (aref vec mid)))
	  (cond ((< obj obj2)
		 (finder obj vec start (1- mid)))
		((> obj obj2)
		 (finder obj vec (1+ mid) end))
		(t 
		 obj)))))))
      
