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

;; 6
(defun intersperser (obj lst)
  "Takes an object OBJ and a list LST and returns a new list
in which the object appears between each pair of elements in
the original list.

> (intersperce '- '(a b c d))
=> (A - B - C - D)"
  (cond ((endp lst)
	 '())
	((>= (length lst) 2)
	 (cons (car lst)
	       (cons obj
		     (intersperser obj (cdr lst)))))
	(t
	 (cons (car lst)
	       (intersperser obj (cdr lst))))))

(defun intersperses (obj lst)
  "Takes an object OBJ and a list LST and returns a new list
in which the object appears between each pair of elements in
the original list.

> (intersperse '- '(a b c d))
=> (A - B - C - D)"
  (let (new-lst)
    (dolist (ls lst (cdr (nreverse new-lst)))
      (setf new-lst (apply #'list ls obj new-lst)))))

(defun intersperse (object list)
    "Takes an OBJECT and a LIST, then returns a new list in which the OBJECT 
appears between each pair of elements in the original LIST.
EXAMPLE
> (intersperse '- '(a b c d))
=> (A - B - C - D)"
  (let ((list-len (length list)))
    (butlast (mapcan #'list
		     list
		     (make-list list-len :initial-element object)))))

;; 7(a)
(defun every-pair-a (lis)
  "Takes a list of numbers LIS and return T IFF the difference
between each successive pair of them is 1; NIL otherwise."
  (assert (and (listp lis)
	       (every #'numberp lis))
	  (lis)
	  " >> Precondition failed: Expecting list of numbers.")
  (let ((len (length lis))
	(ans nil))
    (if (zerop len)
	ans
	(let ((diff (- (cadr lis) (car lis))))
	  (case diff
	    (1
	     (setf ans t)
	     (every-pair-a (cddr lis)))
	    (otherwise (setf ans nil)))
	  ans))))

;; 7(b)
(defun every-pair (predicate list)
  "Apply PREDICATE to each successive pair of elements of the LIST.
Return NIL as soon as any invocation of PREDICATE returns NIL; Otherwise
T if every invocation is T.
Example:
> (EVERY-PAIR #'PLUS1P '(0 1 2 3 4 5 6 7 8 9) => T
> (EVERY-PAIR #'PLUS1P '(1 -0 3 2 5 6 101 100) => T
> (EVERY-PAIR #'PLUS1P '(7 8 7 7 5 6 55 42) => NIL
> (EVERY-PAIR #'PLUS1P '(0 1 2) => NIL"
  (let ((jump 2)
	(len (length list)))
    (do ((first 0 (+ first jump))
	 (second 1 (+ second jump)))
	((or (null (nth first list))
	     (null (nth second list))
	     (endp list))
	 T)
      (unless (funcall predicate (nth first list) (nth second list))
	(return nil)))))

(defun plus1p (x y)
  "Returns T if the difference between X and Y is 1; Otherwise return NIL."
  (or (= 1 (- x y))
      (= 1 (- y x))))
