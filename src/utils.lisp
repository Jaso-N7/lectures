(defpackage :utils
  (:use :cl)
  (:export :tokens
           :tokenize)
  (:documentation "Utility Functions."))

(in-package :utils)

;;; ANSI CL by Paul Graham - Chapter 6

;; 1.
(defun tokens (str &optional (test #'constituent) (start 0))
  "Extract tokens from a string. Given a string STR and a TEST function, 
defaulting to CONSTITUENT, it returns a list of the substrings whose
characters satisfy the function TEST.
START represents where in the substring to begin the extraction.

> (TOKENS \"ab12 3cde.f\" #'alpha-char-p) 
=> (\"ab\" \"cde\" \"f\")

> (TOKENS \"ab12 3cde.f\
            gh\" #'constituent) 
=> (\"ab12\" \"3cde.f\" \"gh\")"
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c)
				   (not (funcall test c)))
			       str
			       :start p1)))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens str test p2)
		    nil)))
	nil)))

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


(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\  ))))



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
      

;; 4
(defun most (fn lst)
  "Returns the element of a list LST with the highest score, according to some
function FN. It returns two values, the highest and second-highest scoring elements
of a list:

> (MOST #'(lambda (e)
	    (length e))
        '((A B) (A B C) (A)))
(A B C)
(A B)

If there is a tie, the element occuring first is returned.

CL-USER> (utils::most #'(lambda (e)
			  (length e))
		      '((a b) (a b c) (d e f) (g)))
		      
(A B C)
(D E F)"
  (let ((sorted (sort (copy-list lst) #'> :key fn)))
    (values (car sorted) (cadr sorted))))
