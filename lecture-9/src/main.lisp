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
