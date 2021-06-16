(defsystem "lecture-7"
  :version "0.1.0"
  :author "Jason S. Robinson"
  :license ""
  :depends-on ()
  :serial t
  :components ((:module "src"
                :components
                ((:file "model")
		 (:file "main"))))

  :description "Student Registry")

(defsystem "lecture-7/tests"
  :author "Jason S. Robinson"
  :license ""
  :depends-on ("lecture-7"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for lecture-7"
  :perform (test-op (op c) (symbol-call :rove :run c)))
