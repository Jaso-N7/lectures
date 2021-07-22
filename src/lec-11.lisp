(defpackage "LECTURES-11"
  (:use :cl)
  (:nicknames :week-11 :lec-11)
  (:documentation "Practical activities / suggestions from Week 11.
Also includes the exercises from Paul Graham's ANSI CL, Chapter 10."))

(in-package "LECTURES-11")

;; 1.(a)
(let ((x 'a)
      (y 'b)
      (z '(c d)))
  (defun 1a ()
    `(,z ,x z))
  (defun 1b ()
    `(x ,y ,@z))
  (defun 1c ()
    `((,@z ,x) z)))

;; 2
(defmacro iffy (predicate when-true &optional (unless-false nil))
  (let ((test (gensym))
	(then (gensym))
	(else (gensym)))
    `(let ((,test ,predicate)
	   (,then ,when-true)
	   (,else ,unless-false))
       (cond (,test
	      ,then)
	     (t  ,else)))))

;; 3
#|
> (let ((n 2))
    (nth-expr n (/ 1 0) (+ 1 2) (/ 1 0)))
3
|#
(defmacro nth-expr (n &rest exprs)
  (let ((index (gensym)))
    `(let ((,index ,n))
       (nth-cdr ,index exprs))))
	      
