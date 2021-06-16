(defpackage :utils
  (:use :cl)
  (:export :bst-insert :bst-find :bst-traverse)
  (:documentation "Supporting utility functions"))

(in-package :utils)

;;; DATA

(defstruct (node (:print-function (lambda (n s d)
				    (declare (ignore d))
				    (format s "#<~A>" (node-elt n)))))
  elt
  (l nil)
  (r nil))

;;; FUNCTIONS

(defun bst-insert (obj bst <)
  "Insert an object OBJ into the provided Binary Search Tree BST;
Otherwise create a new BST."
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
	(cond ((eql obj elt) 
	       bst)
	      ((funcall < obj elt)
	       (make-node :elt elt
			  :l   (bst-insert obj (node-l bst) <)
			  :r   (node-r bst)))
	      (t  (make-node :elt elt
			     :l (node-l bst)
			     :r (bst-insert obj (node-r bst) <)))))))

(defun bst-find (obj bst <)
  "Find Object OBJ in the Binary Search Tree BST.
Return NIL if not found."
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
	(cond ((eql obj elt)
	       bst)
	      ((funcall < obj elt)
	       (bst-find obj (node-l bst) <))
	      (t  (bst-find obj (node-r bst) <))))))

(defun bst-traverse (fn bst)
  "Binary Search Tree traversal."
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))
      
