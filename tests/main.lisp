(defpackage cl-image-hash/tests/main
  (:use :cl
        :cl-image-hash
        :rove))
(in-package :cl-image-hash/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-image-hash)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
