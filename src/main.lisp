(defpackage cl-image-hash
  (:use :cl :opticl)
  (:export :ahash-gen-hash :dhash-gen-hash :hash-dim-bit-diff
		   :hash-dim-bit-match-persent))
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

(defun gray-bytes->list (two-d-bytes)
  (loop for i below (array-dimension two-d-bytes 0)
	 collect (loop for j below (array-dimension two-d-bytes 1)
				collect (aref two-d-bytes i j))))

(defun color-bytes->list (three-d-bytes)
  (loop for i below (array-dimension three-d-bytes 0)
	 collect (loop for j below (array-dimension three-d-bytes 1)
				collect (loop for k below (array-dimension three-d-bytes 2)
						   collect (aref three-d-bytes i j k)))))

(defun color-list->gray-scale (color-list)
  (mapcar #'(lambda (x)
			  (mapcar #'(lambda (y)
						  (ceiling (float (/ (reduce #'+ y :initial-value 0)
											 (length y)))))
					  x))
		  color-list))

(defun byte-2d-list->array (byte-list byte-size)
  (let ((result (make-array `(,(length byte-list)
							   ,(length (car byte-list)))
							:element-type `(unsigned-byte ,byte-size)
							:initial-element 0)))
	(loop for i below (length byte-list)
	   for x in byte-list
	   do (loop for j below (length (car byte-list))
			 for y in x
			 do (setf (aref result i j) y)))
	result))

(defun byte-3d-list->array (byte-list byte-size)
  (let ((result (make-array `(,(length byte-list)
							   ,(length (car byte-list))
							   ,(length (car (car byte-list))))
							:element-type `(unsigned-byte ,byte-size)
							:initial-element 0)))
	(loop for i below (length byte-list)
	   for x in byte-list
	   do (loop for j below (length (car byte-list))
			 for y in x
			 do (loop for k below (length (car (car byte-list)))
				   for z in y
				   do (setf (aref result i j k)
							z))))
	result))

(defun byte-list->array (byte-list &optional (byte-size 8))
  (cond ((and (eq (type-of byte-list) 'CONS)
			  (eq (type-of (car byte-list)) 'CONS)
			  (eq (type-of (car (car byte-list))) 'CONS))
		 (byte-3d-list->array byte-list byte-size))
		((and (eq (type-of byte-list) 'CONS)
			  (eq (type-of (car byte-list)) 'CONS))
		 (byte-2d-list->array byte-list byte-size))
		(t (error "Not 2-3d byte-list"))))

(defun png-file->dim-ary (path)
  (let ((img-bytes (opticl:read-png-file path)))
	(cond ((eq (car (type-of img-bytes)) 'SIMPLE-ARRAY)
		   (let ((dim (length (nth 2 (type-of img-bytes)))))
			 (cond ((= 2 dim) (values (gray-bytes->list img-bytes) 'gray))
				   ((= 3 dim) (values (color-bytes->list img-bytes) 'color))
				   (t (error (format nil "{~S} bytes not support" path))))))
		  (t (error (format nil "{~S} read data is not n-d array" path))))))

(defun dim-ary->png-file (i-path o-path bytes)
  (multiple-value-bind (bytes type)
		(png-file->dim-ary i-path)
	))

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

(defun hash-dim-bit-match-persent (f a-path b-path &optional (len 64))
  (ceiling (* 100 (float (/ (- len (hash-dim-bit-diff f a-path b-path)) len)))))

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
