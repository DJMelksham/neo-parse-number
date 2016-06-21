(declaim (hash-table *white-space-characters* *base-for-exp-marker-hash*))

(define-condition invalid-number (parse-error)
  ((value :reader invalid-number-value
	  :initarg :value
	  :initform nil)
   (reason :reader invalid-number-reason
	   :initarg :reason
	   :initform "Not specified"))
  (:report (lambda (c s)
	     (format s "Invalid number: ~S [Reason: ~A]"
		     (invalid-number-value c)
                     (invalid-number-reason c)))))

(defparameter *white-space-characters*
  (make-hash-table :test #'eql))
(setf (gethash #\Space *white-space-characters*) T
      (gethash #\Tab *white-space-characters*) T
      (gethash #\Return *white-space-characters*) T
      (gethash #\Linefeed *white-space-characters*) T)

(defparameter *base-for-exp-marker-hash*
  (make-hash-table :test #'eql))

(setf (gethash #\d *white-space-characters*) 10.0d0
      (gethash #\D *white-space-characters*) 10.0d0
      (gethash #\f *white-space-characters*) 10.0f0
      (gethash #\F *white-space-characters*) 10.0f0
      (gethash #\s *white-space-characters*) 10.0s0
      (gethash #\S *white-space-characters*) 10.0s0
      (gethash #\l *white-space-characters*) 10.0l0
      (gethash #\L *white-space-characters*) 10.0l0)

(declaim (inline white-space-p))

(defun white-space-p (x wsch)
    (declare (optimize (speed 3) (safety 1))
	     (type character x)
	     (hash-table wsch))
    (gethash x wsch)))

(defun parse-integer-and-places (string start end &key (radix 10))
  (declare (optimize (speed 3) (safety 1))
	   (type string string)
	   (type fixnum start end radix))
  (multiple-value-bind (integer end-pos)
      (if (= start end)
	  (values 0 0)
	  (parse-integer string
			 :start start
			 :end end
			 :radix radix))
    ;; cl:parse-integer will consume trailing whitespace, thus end-pos may be
    ;; larger than the number of digits. Instead of trimming whitespace
    ;; beforehand we count it here
    (let ((relevant-digits (- end-pos start
			      (loop :for pos :from (- end-pos 1) :downto start
				 :while (white-space-p (char string pos) *white-space-characters*)
				 :count 1))))
      (declare (fixnum relevant-digits))
      (cons integer relevant-digits))))

(defun parse-integers (string start end splitting-points &key (radix 10))
  (declare (optimize (speed 3) (safety 1))
	   (type string string)
	   (type fixnum start end radix))
  (values-list (loop for left = start then (1+ right)
		     for point in splitting-points
		     for right = point
		     collect (parse-integer-and-places string
						       left
						       right
						       :radix radix)
		     into integers
		     finally (return
			       (nconc integers
				      (list
				       (parse-integer-and-places string
								 left
								 end
								 :radix radix
								 )))))))

(declaim (inline number-value places))
(defun number-value (x) (car x))
(defun places (x) (cdr x))

(defun parse-number (string &key (start 0) (end (length string)) (radix 10)
                                 ((:float-format *read-default-float-format*)
                                  *read-default-float-format*))
  "Given a string, and start, end, and radix parameters, produce a number according to the syntax definitions in the Common Lisp Hyperspec."
  (flet ((invalid-number (reason)
	   (error 'invalid-number
		  :value (subseq string start end)
		  :reason reason)))
      (if (and (eql (char string start) #\#)
	       (member (char string (1+ start)) '(#\C #\c)))
	  (let ((\(-pos (position #\( string :start start :end end))
		(\)-pos (position #\) string :start start :end end)))
	    (when (or (not \(-pos)
		      (not \)-pos)
		      (position #\( string :start (1+ \(-pos) :end end)
		      (position #\) string :start (1+ \)-pos) :end end))
	      (invalid-number "Mismatched/missing parenthesis"))
	    (let ((real-pos (position-if-not #'white-space-p string
					     :start (1+ \(-pos) :end \)-pos)))
	      (unless real-pos
		(invalid-number "Missing real part"))
	      (let ((delimiting-space (position-if #'white-space-p string
						   :start (1+ real-pos)
						   :end \)-pos)))
		(unless delimiting-space
		  (invalid-number "Missing imaginary part"))
		(let ((img-pos (position-if-not #'white-space-p string
						:start (1+ delimiting-space)
						:end \)-pos)))
		  (unless img-pos
		    (invalid-number "Missing imaginary part"))
		  (let ((img-end-pos (position-if #'white-space-p string
						  :start (1+ img-pos)
						  :end \)-pos)))
		    (complex (parse-real-number string
						:start real-pos
						:end delimiting-space
						:radix radix)
			     (parse-real-number string
						:start img-pos
						:end (or img-end-pos
							 \)-pos)
						:radix radix)))))))
	  (parse-real-number string :start start :end end :radix radix))))

(defun parse-real-number (string &key (start 0) (end nil) (radix 10)
                                      ((:float-format *read-default-float-format*)
                                       *read-default-float-format*))
  "Given a string, and start, end, and radix parameters, produce a number according to the syntax definitions in the Common Lisp Hyperspec -- except for complex numbers."
  (let ((end (or end (length string))))
    (case (char string start)
      ((#\-)
       (* -1 (parse-positive-real-number string
					 :start (1+ start)
					 :end end
					 :radix radix)))
      ((#\#)
       (case (char string (1+ start))
	 ((#\x #\X)
	  (parse-real-number string
			     :start (+ start 2)
			     :end end
			     :radix 16))
	 ((#\b #\B)
	  (parse-real-number string
			     :start (+ start 2)
			     :end end
			     :radix 2))
	 ((#\o #\O)
	  (parse-real-number string
			     :start (+ start 2)
			     :end end
			     :radix 8))
	 (t (if (digit-char-p (char string (1+ start)))
		(let ((r-pos (position #\r string
				       :start (1+ start)
				       :end end
				       :key #'char-downcase)))
		  (unless r-pos
		    (error 'invalid-number
			   :value (subseq string start end)
			   :reason "Missing R in #radixR"))
		  (parse-real-number string
				     :start (1+ r-pos)
				     :end end
				     :radix (parse-integer string
							   :start (1+ start)
							   :end r-pos)))))))
      (t (parse-positive-real-number string
				     :start start
				     :end end
				     :radix radix)))))

(defun base-for-exponent-marker (char)
  (case char
    ((#\d #\D)
     10.0d0)
    ((#\e #\E)
     (coerce 10 *read-default-float-format*))
    ((#\f #\F)
     10.0f0)
    ((#\s #\S)
     10.0s0)
    ((#\l #\L)
     10.0l0)))

(defun make-float/frac (radix exp-marker whole-place frac-place exp-place)
  (let* ((base (base-for-exponent-marker exp-marker))
         (exp (expt base (number-value exp-place))))
    (+ (* exp (number-value whole-place))
       (/ (* exp (number-value frac-place))
          (expt (float radix base)
                (places frac-place))))))

(defun make-float/whole (exp-marker whole-place exp-place)
  (* (number-value whole-place)
     (expt (base-for-exponent-marker exp-marker)
           (number-value exp-place))))

(defun parse-positive-real-number (string &key (start 0) (end nil) (radix 10)
                                               ((:float-format *read-default-float-format*)
                                                *read-default-float-format*))
  "Given a string, and start, end, and radix parameters, produce a number according to the syntax definitions in the Common Lisp Hyperspec -- except for complex numbers and negative numbers."
  (let ((end (or end (length string)))
	(first-char (char string start)))
    (flet ((invalid-number (reason)
	     (error 'invalid-number
		    :value (subseq string start end)
		    :reason reason)))
      (case first-char
	((#\-)
	 (invalid-number "Invalid usage of -"))
	((#\/)
	 (invalid-number "/ at beginning of number"))
	((#\d #\D #\e #\E #\l #\L #\f #\F #\s #\S)
	 (when (= radix 10)
	   (invalid-number "Exponent-marker at beginning of number"))))
      (let (/-pos .-pos exp-pos exp-marker)
	(loop for index from start below end
	      for char = (char string index)
	      do (case char
		   ((#\/)
		    (if /-pos
			(invalid-number "Multiple /'s in number")
			(setf /-pos index)))
		   ((#\.)
		    (if .-pos
			(invalid-number "Multiple .'s in number")
			(setf .-pos index)))
		   ((#\e #\E #\f #\F #\s #\S #\l #\L #\d #\D)
		    (when (= radix 10)
		      (when exp-pos
			(invalid-number
			 "Multiple exponent-markers in number"))
		      (setf exp-pos index)
		      (setf exp-marker (char-downcase char)))))
	      when (eql index (1- end))
	      do (case char
		   ((#\/)
		    (invalid-number "/ at end of number"))
		   ((#\d #\D #\e #\E #\s #\S #\l #\L #\f #\F)
		    (when (= radix 10)
		      (invalid-number "Exponent-marker at end of number")))))
	(cond ((and /-pos .-pos)
	       (invalid-number "Both . and / cannot be present simultaneously"))
	      ((and /-pos exp-pos)
	       (invalid-number "Both an exponent-marker and / cannot be present simultaneously"))
	      ((and .-pos exp-pos)
               (if (< exp-pos .-pos)
		   (invalid-number "Exponent-markers must occur after . in number")
		   (if (/= radix 10)
		       (invalid-number "Only decimal numbers can contain exponent-markers or decimal points")
		       (multiple-value-bind (whole-place frac-place exp-place)
                           (parse-integers string start end
                                           (list .-pos exp-pos)
                                           :radix radix)
                         (make-float/frac radix exp-marker whole-place frac-place exp-place)))))
	      (exp-pos
	       (if (/= radix 10)
		   (invalid-number "Only decimals can contain exponent-markers")
		   (multiple-value-bind (whole-place exp-place)
		       (parse-integers string start end
				       (list exp-pos)
				       :radix radix)
		     (make-float/whole exp-marker whole-place exp-place))))
	      (/-pos
	       (multiple-value-bind (numerator denominator)
		   (parse-integers string start end
				   (list /-pos)
				   :radix radix)
		 (if (>= (number-value denominator) 0)
		     (/ (number-value numerator)
			(number-value denominator))
		     (invalid-number "Misplaced - sign"))))
	      (.-pos
	       (if (/= radix 10)
		   (invalid-number "Only decimal numbers can contain decimal points")
		   (multiple-value-bind (whole-part frac-part)
		       (parse-integers string start end
				       (list .-pos)
				       :radix 10)
		     (cond
                       ((minusp (places frac-part))
                        (if (and (zerop (number-value whole-part))
                                 (zerop (places whole-part)))
                            (invalid-number "Only the . is present")
                            (number-value whole-part)))
                       ((>= (number-value frac-part) 0)
                        (coerce (+ (number-value whole-part)
                                   (/ (number-value frac-part)
                                      (expt 10 (places frac-part))))
                                *read-default-float-format*))
                       (t
                        (invalid-number "Misplaced - sign"))))))
	      (t
	       (values (parse-integer string
				      :start start
				      :end end
				      :radix radix))))))))
