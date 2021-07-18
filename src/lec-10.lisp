(defpackage lectures-10
  (:use :cl)
  (:nicknames "LEC-10")
  (:import-from :utils
		:tokenize)
  (:export :sqrt-of-sum
           :sqrt-sum-sqr
	   :better-equal
           :list-lines
	   :save-no-comments)
  (:documentation "Suggested activity and exercises for Lecture 10."))

(in-package :lectures-10)

;;; DATA

(defparameter *sample-corpus*
  #P"c:/Users/Jason.Robinson/quicklisp/local-projects/lectures/sample-corpus"
  "Sample corpus file used for file operations.")

(defparameter *src-code-commented*
  #P"c:/Users/Jason.Robinson/quicklisp/local-projects/lectures/erlang-src.erl"
  "Sample Elang source code with comments.")

(defparameter *src-code-un-commented*
  #P"c:/Users/Jason.Robinson/quicklisp/local-projects/lectures/readable-erlang-src.erl"
  "Sample Elang source code with no comments.")

;;; FUNCTIONS


;;; 10.8 Practical session / Suggested activity

;; Note that the specification did not mention arity or signedness of the
;; numbers passed.
(defun sqrt-of-sum (&rest args)
  "Takes any number of arguments, and computes the square root of the sum of 
their squares."
  (let ((sum 0))
    (dolist (arg args (sqrt sum))
      (incf sum (expt arg 2)))))

(defun sqrt-sum-sqr (&rest args)
  "Same as `SQRT-OF-SUM'"
  (sqrt (reduce #'+ (mapcar #'(lambda (a)
				(expt a 2))
			    args))))
			    
(defun better-equal (&rest args)
  (let ((head (car args))
	(tail (cdr args)))
    (dolist (a tail T)
      (unless (equal a head)
	(return nil)))))


;;; 10.9 Further reading & exercises

;; Graham Ex 7.1

(defun list-lines (file)
  "Returns a list of strings representing each line in the file FILE."
    (with-open-file (istream file :direction :input)
      (do ((line (read-line istream nil 'eof)
		 (read-line istream nil 'eof)))
	  ((eql line 'eof))
	(print (tokenize line)))))

;; Graham Ex 7.3

(defun save-no-comments (from-file to-file)
  "Writes the contents in FROM-FILE to TO-FILE, minus the lines starting with comments.
Comments are lines starting with %.

EXAMPLE:
CL-USER> (save-no-comments *src-code-commented* *src-code-un-commented*)"
  (with-open-file (istream from-file :direction :input)
    (let ((ostream (open to-file :direction :output
				 :if-exists :overwrite
				 :if-does-not-exist :create)))
      (do ((line (read-line istream nil)
		 (read-line istream nil)))
	  ((null line) (close ostream))
	(unless (char= #\% (char line 0))
	  (princ line ostream))))))
