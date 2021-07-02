(defsystem "lectures"
  :version "0.2.0"
  :author "Jason S. Robinson"
  :license ""
  :depends-on ()
  :serial t
  :components ((:module "src"
                :components
                ((:file "model")
		 (:file "lec-7")
		 (:file "lec-8")
		 (:file "lec-9"))))

  :description "Student Registry")

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
