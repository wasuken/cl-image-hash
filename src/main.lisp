(defpackage cl-image-hash
  (:use :cl :opticl)
  (:export :ahash-gen-hash :dhash-gen-hash :hash-dim-bit-diff))
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

(defun png-file->dim-ary (path)
  (let ((img-bytes (opticl:read-png-file path)))
	(loop for i below (array-dimension img-bytes 0)
	   collect (loop for j below (array-dimension img-bytes 1)
				  collect (aref img-bytes i j)))))

(defun hash-dim-bit-diff (f a-path b-path)
  (let ((a-hash (funcall f a-path))
		(b-hash (funcall f b-path))
		(cnt 0))
	(loop for a in a-hash
	   for b in b-hash
	   do (progn
			(unless (= a b)
			  (incf cnt))))
	cnt))

(defun ahash-gen-hash (path)
  (let* ((img-byte-ary-ary (png-file->dim-ary path))
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

(defun dhash-gen-hash (path)
  (let ((img-byte-ary-ary (png-file->dim-ary path)))
	(flatten
	 (loop for img-byte-ary in img-byte-ary-ary
		collect (loop for i below (1- (length img-byte-ary))
				   collect (if (< (nth i img-byte-ary) (nth (1+ i) img-byte-ary))
							   1
							   0))))))
