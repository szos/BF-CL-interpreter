;;;; brainfuck.lisp

(in-package #:brainfuck)

(defun interpret-bf (string &key initial-offset initial-array
			      (input-stream *standard-input*)
			      (output-stream *standard-output*))
  (let ((array (or initial-array
		   (make-array 30000 :element-type '(unsigned-byte 8)
				     :initial-element 0)))
	(offset (or initial-offset 0))
	(string-position 0)
	(max-position (- (length string) 1))
	(*standard-input* input-stream)
	(*standard-output* output-stream))
    (restart-bind ((retry-interpretation-using-offset
		     (lambda (new-offset)
		       (return-from interpret-bf
			 (interpret-bf string :initial-offset new-offset
					      :initial-array initial-array
					      :input-stream input-stream
					      :output-stream output-stream)))
		     :test-function (lambda (c)
				      (typep c #+sbcl 'sb-int:invalid-array-index-error
					       #-sbcl 'type-error))
		     :interactive-function (lambda ()
					     (format *query-io* "Provide a new offset")
					     (force-output *query-io*)
					     (list (read)))
		     :report-function
		     (lambda (s) (format s "Retry interpretation using a new offset"))))
      (flet ((next-char ()
	       (let ((x (+ string-position 1)))
		 (if (> x max-position)
		     (return-from interpret-bf (values 'finished array offset))
		     (setf string-position x))))
	     (prev-char ()
	       (let ((x (- string-position 1)))
		 (if (< x 0)
		     (return-from interpret-bf (values 'source-error array offset))
		     (setf string-position x))))
	     (incf-at-offset ()
	       (let ((x (aref array offset)))
		 (setf (aref array offset) (or (and (= x 255) 0) (+ x 1)))))
	     (decf-at-offset ()
	       (let ((x (aref array offset)))
		 (setf (aref array offset) (or (and (= x 0) 255) (- x 1))))))
	(tagbody
	   (go bf-dispatch)
	 bf->
	   (incf offset)
	   (go bf-process)
	 bf-<
	   (decf offset)
	   (go bf-process)
	 bf-+
	   (incf-at-offset)
	   (go bf-process)
	 bf--
	   (decf-at-offset)
	   (go bf-process)
	 bf-dot
	   (format *standard-output* "~C" (code-char (aref array offset)))
	   (go bf-process)
	 bf-comma
	   (let ((ch (or (read-char *standard-input* nil) (code-char 0))))
	     (unless (typep (char-code ch) '(integer 0 255))
	       (error "Invalid Character ~A Entered" ch))
	     (setf (aref array offset) (char-code ch))) 
	   (go bf-process)
	 bf-[
	   (when (= 0 (aref array offset))
	     (let ((matches 0))
	       (do ((ch (aref string string-position)
			(aref string string-position)))
		   ((and (or (and (char= ch #\[)
				  (incf matches))
			     t)
			 (and (char= ch #\])
			      (decf matches))
			 (= 0 matches)))
		 (next-char))))
	   (go bf-process)
	 bf-]
	   (unless (= 0 (aref array offset))
	     (let ((matches 0))
	       (do ((ch (aref string string-position)
			(aref string string-position)))
		   ((and (or (and (char= ch #\])
				  (incf matches))
			     t)
			 (and (char= ch #\[)
			      (decf matches))
			 (= 0 matches)))
		 (prev-char))))
	   (go bf-process)
	 bf-process
	   (next-char)
	 bf-dispatch
	   (case (aref string string-position)
	     (#\>  (go bf->))
	     (#\<  (go bf-<))
	     (#\+  (go bf-+))
	     (#\-  (go bf--))
	     (#\.  (go bf-dot))
	     (#\,  (go bf-comma))
	     (#\[  (go bf-[))
	     (#\]  (go bf-]))
	     (otherwise (go bf-process))))))))

(defun bf-repl (&optional (array-size 30000))
  (let ((persistent-array (make-array array-size :element-type '(unsigned-byte 8)
						 :initial-element 0))
	(initial-offset 0))
    (do ((line (read-line *standard-input* nil) (read-line *standard-input* nil)))
	((null line))
      (multiple-value-bind (retval array offset)
	  (interpret-bf line :initial-offset initial-offset :initial-array persistent-array)
	(format t "~&")
	(if (eql retval 'finished)
	    (setf persistent-array array
		  initial-offset offset)
	    (return-from bf-repl))))))
