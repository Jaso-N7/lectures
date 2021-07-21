(defsystem "lectures"
  :version "0.2.0"
  :author "Jason S. Robinson"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "utils")
		 (:file "model")
		 (:file "lec-7"
		  :depends-on ("model"))
		 (:file "lec-8")
		 (:file "lec-9"
		  :depends-on ("utils"))
		 (:file "lec-10")
		 (:file "lec-11"))))
  :in-order-to ((test-op (test-op "lectures/tests")))
  :description "Collection of Practical sessions, suggested activities and Exercises
from Nick Levine's Declarative Languages course and ANSI CL by Paul Graham.")

(defsystem "lectures/tests"
  :author "Jason S. Robinson"
  :license ""
  :depends-on ("lectures"
;	       "ptester"         ; Test harness
	       "cl-quickcheck")  ; Property-Based Testing
  :components ((:module "tests"
                :components
                ((:file "packages")
		 (:file "main"
			:depends-on ("packages")))))
  :description "Test system for the variouse exercises and activities I did from
the lectures"
  :perform (test-op (op c) 
		    (symbol-call 'lectures/tests ':run-all-tests)))
