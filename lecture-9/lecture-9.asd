(defsystem "lecture-9"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Suggested activity and exercises from Declarative Languages: Lecture 9"
  :in-order-to ((test-op (test-op "lecture-9/tests"))))

(defsystem "lecture-9/tests"
  :author ""
  :license ""
  :depends-on ("lecture-9"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for lecture-9"
  :perform (test-op (op c) (symbol-call :rove :run c)))
