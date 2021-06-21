(defpackage lecture-8/tests/main
  (:use :cl
        :lecture-8
        :rove))
(in-package :lecture-8/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :lecture-8)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
