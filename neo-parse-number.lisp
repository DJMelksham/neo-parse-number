(in-package :neo-parse-number)

(defparameter *default-float-format* 'double-float)
(defparameter *radix* 10)
(defparameter *default-value-on-error* nil)

(defun parse-int (string &optional (value-on-error *default-value-on-error*) (radix *radix*) (start 0) (end nil))
  "Skips initial and trailing non-relevent characters and tries to parse an integer."

  (handler-case 
      (let ((minusp 0)
	    (string-length 0)
	    (i 0)
	    (result 0)
	    (char-buff #\0)
	    (digit-buff 0))

	(declare (optimize (speed 3))
		 (fixnum i string-length minusp radix start)
		 (integer result)
		 (string string)
		 (character char-buff)
		 (t digit-buff)
		 (dynamic-extent string-length minusp char-buff digit-buff))
	
	(setf string-length (if end
				end
				(length string))
	      i start
	      char-buff (char string i))

	(cond ((char= char-buff #\-)(progn
				      (setf minusp 1)
				      (incf i)))
	      ((char= char-buff #\+)(incf i))
	      (t nil))
	
	(loop until (or (eql string-length i)
			(not (setf digit-buff (digit-char-p (char string i) radix))))
	   do (setf result (+ (* result radix) digit-buff))	   
	   do (incf i))
	
	(if (eql minusp 1)
	    (values (- result) (- i start))
	    (values result (- i start))))
    (error () (values value-on-error 0))))

(defun parse-ratio (string &optional (value-on-error *default-value-on-error*))
  "Parse a string as a fraction."
  (handler-case 
      (let ((divider-position (position #\/ string)))
	(/ (parse-integer string :start 0 :end divider-position)
	   (parse-integer string :start (+ divider-position 1))))
    (error () value-on-error)))

(defun parse-float (string &optional (value-on-error *default-value-on-error*))
  "Parse a string as a float if we deem it reasonably possible to do so."
  (handler-case 
      (let ((string-length 0) ;length of string to terminate early if done
	    (length-marker 0) ;keeps track of where we are in the string while parsing
	    (whole-part 0) ;stores the whole-number part of the float
	    (whole-length 0) ;keeps track of how many digits are in the whole-number part
	    (fraction-part 0) ;keeps track of the fraction part of the float
	    (fraction-length 0) ;keeps track of how many digits are in the fraction part
	    (exponent-value 0) ;stores the exponent of the float
	    (exponent-length 0) ;keeps track of how many digits are in the exponent
	    (type-of-float 'DOUBLE-FLOAT))

	(declare (optimize (speed 3)(safety 0))
		 (fixnum string-length
			 length-marker
			 whole-part
			 whole-length
			 fraction-part
			 fraction-length
			 exponent-value
			 exponent-length )
		 (symbol type-of-float)
		 (string string)
		 (dynamic-extent string-length
				 whole-part
				 whole-length
				 fraction-part
				 fraction-length
				 exponent-value
				 exponent-length))

	(setf string-length (length string))

	;; First, we attempt to parse an integer from the left-most part of the string
	;; storing the value and the length of the parsed integer in the respective
	;; values and incrementing our place in the full string
	;; This constitutes the whole part of the floating point number

	(multiple-value-setq (whole-part whole-length) (parse-int string nil 10))
	(incf length-marker whole-length) ;;increment relative position by length of first int

	;; If our previous operation didn't parse the entire string in one go,
	;; we increment our relative position in the string by 1 (within the subseq)
	;; in order to avoid the period in a floating point representation
	;; and parse a second integer from that point on.
	;; This consistutes the fractional part of the floating point number
	
	(if (not (eql string-length length-marker))
	    (progn
	      (multiple-value-setq (fraction-part fraction-length)
		(parse-int string nil 10 (incf length-marker)))
	      (incf length-marker fraction-length)))

	;; If our previous operation didn't parse the entire string yet,
	;; we now deal with identifying exponent markers
	;; if none are present, or if the ambiguous
	;; character of #\e or #\E is present, we use a default
	;; value (by convention, a double float)
	;; We coerce the other numbers we use in our final calculation
	;; to the same type
	
	(if (not (eql length-marker string-length))
	    (progn
	      (setf type-of-float (case (char string length-marker)
				    ((#\d #\D) 'DOUBLE-FLOAT)
				    ((#\e #\E) *default-float-format*)
				    ((#\f #\F) 'FLOAT)
				    ((#\s #\S) 'SHORT-FLOAT)
				    ((#\l #\L) 'LONG-FLOAT)
				    (t *default-float-format*)))
	      
	      (multiple-value-setq (exponent-value exponent-length)
		(parse-int string nil 10 (incf length-marker)))))

	;; Now we built up the final float
	;; Note how we have to check whether it is a positive or negative float
	;; because adding a positive fractional-part to negative whole-part
	;; moves the representation in the wrong direction

	(coerce (* (+ (* whole-part (the fixnum (expt 10 (the unsigned-byte fraction-length))))
		      (if (minusp whole-part)
			  (- fraction-part)
			  fraction-part))
		   (expt 10 (the fixnum (- exponent-value fraction-length))))
		type-of-float))
    
    (error () value-on-error)))  

(defun parse-double! (string &optional (value-on-error *default-value-on-error*))
  "Parse a string as a DOUBLE-FLOAT if we deem it reasonably possible to do so."
  (handler-case 
      (let ((string-length (length string)) ;length of string to terminate early if done
	    (whole-part 0) ;stores the whole-number part of the float
	    (whole-length 0) ;keeps track of how many digits are in the whole-number part
	    (length-marker 0) ;keeps track of where we are in the string while parsing
	    (fraction-part 0) ;keeps track of the fraction part of the float
	    (fraction-length 0) ;keeps track of how many digits are in the fraction part
	    (exponent-value 0) ;stores the exponent of the float
	    (exponent-length 0) ;keeps track of how many digits are in the exponent
	    (exponent-base 10.0d0) ;base
	    (type-of-float 'DOUBLE-FLOAT)
	    (coercion-coefficient 1.0d0))

	(declare (fixnum string-length whole-part fraction-part length-marker whole-length fraction-length exponent-length)
		 (type (signed-byte 32) fraction-length exponent-value)
		 (symbol type-of-float)
		 (simple-array string)
		 (double-float exponent-base coercion-coefficient)
		 (dynamic-extent string-length
				 whole-part
				 whole-length
				 length-marker
				 fraction-part
				 fraction-length
				 exponent-value
				 exponent-length
				 exponent-base
				 coercion-coefficient))

	;; First, we attempt to parse an integer from the left-most part of the string
	;; storing the value and the length of the parsed integer in the respective
	;; values and incrementing our place in the full string
	;; This constitutes the whole part of the floating point number

	(multiple-value-setq (whole-part whole-length) (parse-integer string :junk-allowed t))
	(incf length-marker whole-length) ;;increment relative position by length of first int

	;; If our previous operation didn't parse the entire string in one go,
	;; we increment our relative position in the string by 1 (within the subseq)
	;; in order to avoid the period in a floating point representation
	;; and parse a second integer from that point on.
	;; This consistutes the fractional part of the floating point number
	
	(if (not (eql string-length length-marker))
	    (progn
	      (multiple-value-setq (fraction-part fraction-length)
		(parse-integer (subseq string (incf length-marker)) :junk-allowed t))
	      (incf length-marker fraction-length)))

	;; If our previous operation didn't parse the entire string yet,
	;; we now deal with identifying exponent markers.
	;; Because we are using parse-double, we won't check for exact
	;; values, because we'll interpret everything liberally as an
	;; exponent marker that really means a double float.
	;; No numbers need to be coerced, because we already know what
	;; type they will be.
	
	(if (not (eql length-marker string-length))
	    (progn
	      (multiple-value-setq (exponent-value exponent-length) (parse-integer (subseq string (incf length-marker))))
	      (incf length-marker exponent-length)))

	;; Now we built up the final float
	;; Note how we have to check whether it is a positive or negative float
	;; because adding a positive fractional-part to negative whole-part
	;; moves the representation in the wrong direction

	(if (minusp whole-part)
	    (* (- (coerce whole-part type-of-float)
		  (* (coerce fraction-part type-of-float) (/ coercion-coefficient (expt exponent-base fraction-length))))
	       (expt exponent-base exponent-value))
	    (* (+ (coerce whole-part type-of-float)
		  (* (coerce fraction-part type-of-float) (/ coercion-coefficient (expt exponent-base fraction-length))))
	       (expt exponent-base exponent-value))))
	    (error () value-on-error)))

(defun parse-single! (string &optional (value-on-error *default-value-on-error*))
    (handler-case 
      (let ((string-length (length string)) ;length of string to terminate early if done
	    (whole-part 0) ;stores the whole-number part of the float
	    (whole-length 0) ;keeps track of how many digits are in the whole-number part
	    (length-marker 0) ;keeps track of where we are in the string while parsing
	    (fraction-part 0) ;keeps track of the fraction part of the float
	    (fraction-length 0) ;keeps track of how many digits are in the fraction part
	    (exponent-value 0) ;stores the exponent of the float
	    (exponent-length 0) ;keeps track of how many digits are in the exponent
	    (exponent-base 10.0s0) ;base
	    (type-of-float 'SINGLE-FLOAT)
	    (coercion-coefficient 1.0s0))

	(declare (fixnum string-length whole-part fraction-part length-marker whole-length fraction-length exponent-length)
		 (type (signed-byte 32) fraction-length exponent-value)
		 (symbol type-of-float)
		 (simple-array string)
		 (single-float exponent-base coercion-coefficient)
		 (dynamic-extent string-length
				 whole-part
				 whole-length
				 length-marker
				 fraction-part
				 fraction-length
				 exponent-value
				 exponent-length
				 exponent-base
				 coercion-coefficient))

	;; First, we attempt to parse an integer from the left-most part of the string
	;; storing the value and the length of the parsed integer in the respective
	;; values and incrementing our place in the full string
	;; This constitutes the whole part of the floating point number

	(multiple-value-setq (whole-part whole-length) (parse-integer string :junk-allowed t))
	(incf length-marker whole-length) ;;increment relative position by length of first int

	;; If our previous operation didn't parse the entire string in one go,
	;; we increment our relative position in the string by 1 (within the subseq)
	;; in order to avoid the period in a floating point representation
	;; and parse a second integer from that point on.
	;; This consistutes the fractional part of the floating point number
	
	(if (not (eql string-length length-marker))
	    (progn
	      (multiple-value-setq (fraction-part fraction-length)
		(parse-integer (subseq string (incf length-marker)) :junk-allowed t))
	      (incf length-marker fraction-length)))

	;; If our previous operation didn't parse the entire string yet,
	;; we now deal with identifying exponent markers.
	;; Because we are using parse-single, we won't check for exact
	;; values, because we'll interpret everything liberally as an
	;; exponent marker that really means a double float.
	;; No numbers need to be coerced, because we already know what
	;; type they will be.
	
	(if (not (eql length-marker string-length))
	    (progn
	      (multiple-value-setq (exponent-value exponent-length) (parse-integer (subseq string (incf length-marker))))
	      (incf length-marker exponent-length)))

	;; Now we built up the final float
	;; Note how we have to check whether it is a positive or negative float
	;; because adding a positive fractional-part to negative whole-part
	;; moves the representation in the wrong direction

	(if (minusp whole-part)
	    (* (- (coerce whole-part type-of-float)
		  (* (coerce fraction-part type-of-float) (/ coercion-coefficient (expt exponent-base fraction-length))))
	       (expt exponent-base exponent-value))
	    (* (+ (coerce whole-part type-of-float)
		  (* (coerce fraction-part type-of-float) (/ coercion-coefficient (expt exponent-base fraction-length))))
	       (expt exponent-base exponent-value))))
	    (error () value-on-error)))

(defun parse-complex-num (string &optional (value-on-error *default-value-on-error*))
  (handler-case (parse-number:parse-number string)
    (error () value-on-error)))

(defun parse-string (string &optional (value-on-error *default-value-on-error*))
  "Return a deep copy of the input string"
  (if (stringp string)
      (let* ((string-length (length string))
	     (new-string (make-array string-length :element-type 'character)))
	(loop
	   for i from 0 to string-length
	   do (setf (schar new-string i) (aref string i)))
	new-string)
      value-on-error))
  
(defun parse-type-from-string (string)
  "Obtain the most sensible type of number contained in a string"
  (let ((string-num-type (type-of (handler-case (parse-number:parse-number string)
				    (error () nil)))))
    (cond ((eql #\. (char string (- (length string) 1))) 'STRING)
	  ((eq string-num-type 'BIT) 'INTEGER)
	  ((eq string-num-type 'FIXNUM) 'INTEGER)
	  ((and (listp string-num-type)
		(eq (car string-num-type) 'INTEGER)) 'INTEGER)
	  ((eq string-num-type 'FLOAT) 'FLOAT)
	  ((eq string-num-type 'BIGNUM) 'INTEGER)
	  ((eq string-num-type 'SINGLE-FLOAT) 'FLOAT)
	  ((eq string-num-type 'DOUBLe-FLOAT) 'FLOAT)
	  ((eq string-num-type 'SHORT-FLOAT) 'FLOAT)
	  ((eq string-num-type 'LONG-FLOAT) 'FLOAT)
	  ((eq string-num-type 'RATIO) 'RATIO)
	  ((and (listp string-num-type)
		(eq (car string-num-type) 'COMPLEX)) 'COMPLEX)
	  (t 'STRING))))
  
(defun parse-number (string &optional (value-on-error *default-value-on-error*) (radix *radix*))
  (let ((type-of-string (parse-type-from-string string)))
    (cond ((eq type-of-string 'FLOAT)(parse-float string value-on-error))
	  ((eq type-of-string 'INTEGER)(parse-int string value-on-error radix))
	  ((eq type-of-string 'RATIO)(parse-ratio string value-on-error))
	  ((eq type-of-string 'COMPLEX)(parse-complex-num string value-on-error))
	  ((eq type-of-string 'STRING)(loop
					 with new-string = (make-array (length string) :element-type 'character)
					 for i from 0 to (length string)
					 for char across string
					 do (setf (aref new-string i) char)
					 return new-string))
	  (t value-on-error))))


