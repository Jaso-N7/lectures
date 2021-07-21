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
(defmacro iffy (predicate when-true unless-false)
  (let ((test (gensym))
	(then (gensym))
	(else (gensym)))
    `(let ((,test ,predicate)
	   (,then ,when-true)
	   (,else ,unless-false))
       (cond (,test
	      ,then)
	     (t  ,else)))))
	      
