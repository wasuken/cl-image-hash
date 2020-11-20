(defpackage cl-image-hash
  (:use :cl :opticl)
  (:export :ahash-gen-hash
		   :dhash-gen-hash
		   :hash-dim-bit-diff
		   :hash-dim-bit-match-persent
		   :byte-array-shrink
		   :png-file->dim-list))
(in-package :cl-image-hash)

;;; (ql:quickload '(:opticl :cl-debug-print))
;;; (cl-syntax:use-syntax cl-debug-print:debug-print-syntax)

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

(defun byte-2d-array->list (byte-array)
  (let* ((size-lst (nth 2 (type-of byte-array)))
		 (x (car size-lst))
		 (y (nth 1 size-lst)))
	(loop for i below x
	   collect (loop for j below y
				  collect (aref byte-array i j)))))

(defun byte-3d-array->list (byte-array)
  (let* ((size-lst (nth 2 (type-of byte-array)))
		 (x (car size-lst))
		 (y (nth 1 size-lst))
		 (z (nth 2 size-lst)))
	(loop for i below x
	   collect (loop for j below y
				  collect (loop for k below z
							 collect (aref byte-array i j k))))))

(defun byte-array->list (byte-array)
  (let ((dim (length (nth 2 (type-of byte-array)))))
	(cond ((= 3 dim)
		   (byte-3d-array->list byte-array))
		  ((= 2 dim)
		   (byte-2d-array->list byte-array))
		  (t (error "Not 2-3d byte-list")))))

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

(defun png-file->dim-list (path)
  (let ((img-bytes (opticl:read-png-file path)))
	(cond ((eq (car (type-of img-bytes)) 'SIMPLE-ARRAY)
		   (let ((dim (length (nth 2 (type-of img-bytes)))))
			 (cond ((= 2 dim) (values (gray-bytes->list img-bytes) 'gray))
				   ((= 3 dim) (values (color-bytes->list img-bytes) 'color))
				   (t (error (format nil "{~S} bytes not support" path))))))
		  (t (error (format nil "{~S} read data is not n-d array" path))))))

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
  (let* ((img-byte-array-array (png-file->dim-list path))
		 (img-avgs (loop for img-byte-array in img-byte-array-array
					  collect (floor
							   (float (/ (reduce #'+ img-byte-array :initial-value 0)
										 (length img-byte-array)))))))
	(flatten
	 (mapcar #'(lambda (img-byte-array avg)
				 (mapcar #'(lambda (x) (if (< avg x) 1 0))
						 img-byte-array))
			 img-byte-array-array
			 img-avgs))))

(defun dhash-gen-hash (path)
  (let ((img-byte-array-array (png-file->dim-list path)))
	(flatten
	 (loop for img-byte-array in img-byte-array-array
		collect (loop for i below (1- (length img-byte-array))
				   collect (if (< (nth i img-byte-array) (nth (1+ i) img-byte-array))
							   1
							   0))))))

(defun byte-list-trim (byte-list begin-width end-width begin-height end-height)
  (loop for x from begin-width to end-width
	 until (or (< (length byte-list) x)
			   (null (nth x byte-list)))
	 collect (loop for y from begin-height to end-height
				until (or (< (length byte-list) x)
						  (< (length (car byte-list)) y)
						  (null (nth y (nth x byte-list))))
				collect (nth y (nth x byte-list)))))

(defun byte-array-shrink (byte-array shrink-width shrink-height)
  (let* ((byte-list (byte-array->list byte-array))
		 (array-width (length byte-list))
		 (array-height (length (car byte-list)))
		 (shrink-range-width (ceiling (float (/ array-width shrink-width))))
		 (shrink-range-height (ceiling (float (/ array-height shrink-height)))))
	(loop for i from 0 to (1- array-width) by shrink-range-width
	   collect (loop for j from 0 to (1- array-height) by shrink-range-height
				  collect (caar (byte-list-trim byte-list
												i
												(1- (+ i shrink-range-width))
												j
												(1- (+ j shrink-range-height))))))))

(defun byte-array-shrink-8x8 (byte-array)
  (byte-array-shrink byte-array 8 8))

(defun byte-array-shrink-9x8 (byte-array)
  (byte-array-shrink byte-array 9 8))

(defun dct-1d (seq k)
  (let* ((sum 0)
		 (n (length seq))
		 (last-n (nth (1- n) seq)))
	(loop for i from 1 to (- n 2)
	   do (setf sum
				(+ sum (+ (* (nth i seq) (cos (* (/ 3.14 (1- n)) n k)))
						  (* (/ (expt -1 k) 2) last-n)))))
	(float (+ (/ (nth 0 seq) 2) sum))))

(defun dct-2d-c (p)
  (if (zerop p)
	  (/ 1 (sqrt 2))
	  1))

;;; 参考
;;; https://jp.mathworks.com/help/signal/ref/dct.html#References
(defun dct-2d (seq)
  (let* ((single-dim-seq (flatten seq))
		 (end (1- (length single-dim-seq)))
		 (len (length single-dim-seq)))
	(loop for k from 0 to end
	   collect (let ((rst 0)
					 (c (if (= k 0)
							(/ 1.0 (sqrt len))
							(sqrt (/ 2.0 len)))))
				 (loop for n from 0 to end
					do (incf rst (* (nth n single-dim-seq)
									(cos (* (/ pi (1- len))
											(1- k)
											(1- (* 2 n)))))
							 ))
				 (* c rst)))
	))

;;; 参考
;;; https://jp.mathworks.com/help/signal/ref/idct.html#bvk_uaj
(defun idct-2d (seq)
  (let* ((single-dim-seq (flatten seq))
		 (end (1- (length single-dim-seq)))
		 (len (length single-dim-seq)))
	(loop for k from 0 to end
	   collect (let ((rst 0)
					 (c (if (= k 0)
							(/ 1.0 (sqrt len))
							(sqrt (/ 2.0 len)))))
				 (loop for n from 0 to end
					do (incf rst (* (nth n single-dim-seq)
									(cos (* (/ pi (1- len))
											(1- n)
											(1- (* 2 k)))))
							 ))
					 (* c rst)))
		 ))
