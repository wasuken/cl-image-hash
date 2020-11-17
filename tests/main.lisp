(defpackage cl-image-hash/tests/main
  (:use :cl
        :cl-image-hash
        :rove
		:opticl))
(in-package :cl-image-hash/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-image-hash)' in your Lisp.
(deftest byte-array-shrink-test
	(testing "Size tests"
			 (loop for i from 8 to 10
				do (loop for j from 8 to 12
					  do (let ((result (byte-array-shrink
										(opticl:read-png-file "./hana.png") i j)))
						   (unless (and (= i (length result))
										(= j (length (car result))))
							 (format t "i:~A = len:~A, j:~A = len: ~A~%"
									 i (length result)
									 j (length (car result))))
						   (ok (and (= i (length result))
									(= j (length (car result))))))))))
