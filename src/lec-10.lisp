(defpackage lectures-10
  (:use :cl)
  (:nicknames "LEC-10")
  (:export :sqrt-of-sum
           :sqrt-sum-sqr
	   :better-equal
	   :list-lines)
  (:documentation "Suggested activity and exercises for Lecture 10."))

(in-package :lectures-10)

;;; DATA

(defparameter *sample-corpus*
  #P"c:/Users/Jason.Robinson/quicklisp/local-projects/lectures/sample-corpus"
  "Sample corpus file used for file operations.")

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


;; Credit goes to INF4820
;; Taken from: https://www.uio.no/studier/emner/matnat/ifi/nedlagte-emner/INF4820/h17/exercises/exercise1.pdf
(defun tokenize (string)
  "Break up a line of text into a list of tokens (word-like units)"
  (loop
	for start = 0 then (1+ space)
	for space = (position #\SPACE string :start start)
	for token = (subseq string start space)
	unless (string= token "")
	  collect token
	until (not space)))
			   
