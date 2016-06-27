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

(declaim (inline white-space-p))

(defun white-space-p (char)
    (declare (optimize (speed 3) (safety 1))
	     (type character char))
    (case char
      ((#\Space #\Return #\Tab #\Linefeed)
       T)))

(declaim (inline base-for-exponent-marker))

(defun base-for-exponent-marker (char)
  (declare (optimize (speed 3) (safety 1)))
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
				 :while (white-space-p (char string pos))
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
    ;; Checking to see if the string starts with the #c signifier for complex numbers
    (if (and (eql (char string start) #\#)
	     (member (char string (1+ start)) '(#\C #\c)))
	;; Complex number check - if it is looking like a complex number
	;; set local variables indicating the position of the starting
	;; and ending parentheses
	(let ((\(-pos (position #\( string :start start :end end))
	      (\)-pos (position #\) string :start start :end end)))
	  ;; Complex number stuff - checking for mismatched parentheses
	  ;; and throwing an error if they're found to be such
	  (when (or (not \(-pos)
		    (not \)-pos)
		    (position #\( string :start (1+ \(-pos) :end end)
		    (position #\) string :start (1+ \)-pos) :end end))
	    (invalid-number "Mismatched/missing parenthesis"))
	  ;; Complex number stuff - get the position of the first number
	  ;; being the position of the first non-white-space character
	  ;; in the string, after the opening paren's position.
	  (let ((real-pos (position-if-not #'white-space-p string
					   :start (1+ \(-pos) :end \)-pos)))
	    ;; Complex number stuff - if the first number, that is to say
	    ;; the real part of the complex number, cannot be found,
	    ;; return an error saying there is a missing real part.
	    (unless real-pos
	      (invalid-number "Missing real part"))
	    ;; Complex number stuff - if the first part of the complex
	    ;; number has been found, then we go looking for the white
	    ;; space that delimits the real and imaginary part
	    (let ((delimiting-space (position-if #'white-space-p string
						 :start (1+ real-pos)
						 :end \)-pos)))
	      ;; and if we can't find it, we say there's no imaginary part
	      (unless delimiting-space
		(invalid-number "Missing imaginary part"))
	      ;; Complex number stuff - Then we get the position of the
	      ;; imaginary part, which is the first position of non-whitespace
	      ;; characters AFTER the original intermediary white space has been
	      ;; found, understanding that it must have been found, because
	      ;; we would have thrown an error if it hadn't by this point.
	      (let ((img-pos (position-if-not #'white-space-p string
					      :start (1+ delimiting-space)
					      :end \)-pos)))
		;; If the imaginary part is not found, throw an error
		(unless img-pos
		  (invalid-number "Missing imaginary part"))
		;; Complex number stuff - get the end point of the imaginary
		;; part, which is to say, searching between the imaginary number
		;; and the point of the closing brackets for a complex number
		;; We do this by searching for white space, but it might
		;; be that we find no white space.  If this is the case,
		;; its okay, because we will call the following complex function
		;; with an or clause, so that if there is no end pos, we'll
		;; simply read the imaginary part by reading up to the closing bracket
		;; which is fed in as a parameter instead.
		
		(let ((img-end-pos (position-if #'white-space-p string
						:start (1+ img-pos)
						:end \)-pos)))
		  ;; And now call the complex function
		  (complex (parse-real-number string
					      :start real-pos
					      :end delimiting-space
					      :radix radix)
			   (parse-real-number string
					      :start img-pos
					      :end (or img-end-pos
						       \)-pos)
					      :radix radix)))))))
	;; If the number doesn't look like a complex number, parse it with
	;; a call to parse-real-number and some parameters
	(parse-real-number string :start start :end end :radix radix))))

(defun parse-real-number (string &key (start 0) (end nil) (radix 10)
				   ((:float-format *read-default-float-format*)
				    *read-default-float-format*))
  "Given a string, and start, end, and radix parameters, produce a number according to the syntax definitions in the Common Lisp Hyperspec -- except for complex numbers."
  (let ((end (or end (length string))))
    ;; Real number parsing: Is the first character a minus sign
    ;; If so, parse the positive number to the right of the minus
    ;; sign and multiply the result by -1
    (case (char string start)
      ((#\-)
       (* -1 (parse-positive-real-number string
					 :start (1+ start)
					 :end end
					 :radix radix)))
      ;;Real number parsing: If the first character is a hash then we check for the other
      ;; characters that signal different radixes, parsing the remainder of the string
      ;; with the original parse-real-number-function recursively
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

	 ;; If the first character was a hash, and there's numbers following it
	 ;; then we need to check for the radix signifier, which is the #\r or #\R
	 ;; character.  If we can't find it, then an error is signalled.
	 ;; If we do find it, then we parse the real number of the remaining part
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
