(defsystem "lecture-8"
  :version "0.1.0"
  :author "Jason Robinson"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Practical activity for lecture 8."
  :in-order-to ((test-op (test-op "lecture-8/tests"))))

(defsystem "lecture-8/tests"
  :author "Jason Robinson"
  :license ""
  :depends-on ("lecture-8"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for lecture-8"
  :perform (test-op (op c) (symbol-call :rove :run c)))
