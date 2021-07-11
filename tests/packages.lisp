(defpackage lectures/tests
  (:use :cl
	:utils
	:model
	:lectures-7
        :lectures-8 
        :lectures-9
        :lectures-10 
	:cl-quickcheck)
  (:export :run-all-tests
	   :run-unit-tests
	   :run-quickchecks)
  (:documentation "Test package for Lectures."))
