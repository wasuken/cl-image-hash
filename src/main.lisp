(defpackage cl-image-hash
  (:use :cl :opticl))
(in-package :cl-image-hash)

(defun flatten (obj)
  (cond ((eq (type-of obj) 'CONS)
		 (if (null (cdr obj))
			 (flatten (car obj))
			 (append (flatten (car obj))
					 (flatten (cdr obj)))))
		((eq (type-of obj) 'NIL)
		 '())
		(t `(,obj))))

(defun ahash-read-png (path)
  (let ((img-bytes (opticl:read-png-file path)))
	(loop for i below (array-dimension img-bytes 0)
	   collect (loop for j below (array-dimension img-bytes 1)
				  collect (aref img-bytes i j)))))

(defun ahash-gen-hash (path)
  (let* ((img-byte-ary-ary (read-png path))
		 (img-avgs (loop for img-byte-ary in img-byte-ary-ary
					  collect (floor
							   (float (/ (reduce #'+ img-byte-ary :initial-value 0)
										 (length img-byte-ary)))))))
	(flatten
	 (mapcar #'(lambda (img-byte-ary avg)
				 (mapcar #'(lambda (x) (if (< avg x) 1 0))
						 img-byte-ary))
			 img-byte-ary-ary
			 img-avgs))))
