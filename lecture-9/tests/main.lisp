(defpackage lecture-9/tests/main
  (:use :cl
        :lecture-9
        :rove))
(in-package :lecture-9/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :lecture-9)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
