(defpackage lecture-9
  (:use :cl)
  (:documentation ""))

(in-package :lecture-9)

;;; ANSI CL - Paul Graham Chapter 5 Exercises

;; 1.(a)
;; ((lambda (x)
;;    (cons x x))
;;  (car y))

;; 1.(b)
;; ((lambda (w)
;;    ((lambda (y)
;;       (cons w y))
;;     (+ w z)))
;;  z)

;; 2.
(defun mystery (x y)
  "Determines the position of X in list Y.
Example 1:
> (LECTURE-9::MYSTERY 5 '(1 9 3 4 5 6 7 8)) => 4
Example 2:
> (LECTURE-9::MYSTERY 5 '(A B C D E F 0 5 9 2)) => 7"
  (cond ((null y)
	 nil)
	((eql (car y) x)
	 0)
	(t
	 (let ((z (mystery x (cdr y))))
	   (and z (+ z 1))))))

;; 3.
(defun sqr>5 (n)
  "Computes the square of N iff N is greater than five(5).
Otherwise return NIL.
EXAMPLES:
CL-USER> (LECTURE-9::SQR>5 3)
NIL
CL-USER> (LECTURE-9::SQR>5 30)
900
"
  (when (> n 5)
    (* n n)))


(defun precedes-rec (x v)
  "Takes an object X and vector V, and returns a list of
all the objects that immediately precede X in V:
> (precede #\a \"abracadraba\") => (#\c #\d #\r)"
  (let* ((pos (1- (length v)))
	 (lst (remove-duplicates 	; I don't like this solution
	       (precedes-rec-aux x v pos))))
    (sort lst #'char<)))

(defun precedes-rec-aux (x v pos)
  "Helper function to `PRECEDES-REC'. Takes an object X, vector V and current
position in vector POS. Traverses the vector and returns a list of all
the objects that immediately preceds X in V."
    (cond ((zerop pos)
	   '())
	  ((and (eql x (aref v pos))
		(not (eql x (aref v (1- pos)))))
	   (cons (aref v (1- pos))
		 (precedes-rec-aux x v (1- pos))))
	  (t
	    (precedes-rec-aux x v (1- pos)))))
	
(defun precedes (x v)
  "Takes an object X and vector V, and returns a list of
all the objects that immediately precede X in V:
> (precede #\a \"abracadraba\") => (#\c #\d #\r)"
  (let ((pre '())
	(lenv (length v)))
    (do ((i 2 (+ i 1)))
	((= i lenv) (sort pre #'char<))
      (when (and (eql x (aref v i))
		 (not (eql x (aref v (- i 1)))))
	(pushnew (aref v (- i 1)) pre)))))
