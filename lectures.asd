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
			:depends-on ("utils")))))

  :description "Collection of Practical sessions, suggested activities and Exercises
from Nick Levine's Declarative Languages course and ANSI CL by Paul Graham.")

(defsystem "lectures/tests"
  :author "Jason S. Robinson"
  :license ""
  :depends-on ("lectures"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for lectures"
  :perform (test-op (op c) (symbol-call :fiveam :run c)))
