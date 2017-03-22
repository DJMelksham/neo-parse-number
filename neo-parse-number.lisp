(in-package :neo-parse-number)

(defparameter *default-float-format* 'double-float)

(declaim (inline white-space-p open-paren-p close-paren-p))

(defun white-space-p (char)
  (declare (optimize (speed 3)(safety 0))
	   (character char))
  (if (or (char= char #\Space)
	  (char= char #\Tab)
	  (char= char #\Return)
	  (char= char #\Linefeed))
      T
      NIL))

(defun open-paren-p (char)
  (char= char #\())

(defun close-paren-p (char)
  (char= char #\)))

(defun parse-int (string &key (value-on-error nil) (radix 10) (start 0) (end nil))
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

(defun parse-ratio (string &key (value-on-error nil)(start 0)(end nil))
  "Parse a string as a fraction."
  (handler-case 
      (let ((divider-position (position #\/ string :start start :end end)))
	(/ (parse-integer string :start 0 :end divider-position)
	   (parse-integer string :start (+ divider-position 1) :end end)))
    (error () value-on-error)))

(defun parse-float (string &key (value-on-error nil) (start 0) (end nil))
  "Parse a string as a float if we deem it reasonably possible to do so."
  (handler-case 
      (let ((string-length 0) ;length of string to terminate early if done
	    (length-marker start) ;keeps track of where we are in the string while parsing
	    (whole-part 0) ;stores the whole-number part of the float
	    (whole-length 0) ;keeps track of how many digits are in the whole-number part
	    (fraction-part 0) ;keeps track of the fraction part of the float
	    (fraction-length 0) ;keeps track of how many digits are in the fraction part
	    (exponent-value 0) ;stores the exponent of the float
	    (exponent-length 0) ;keeps track of how many digits are in the exponent
	    (type-of-float 'DOUBLE-FLOAT))

	(declare (optimize (speed 3))
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
				 length-marker
				 whole-part
				 whole-length
				 fraction-part
				 fraction-length
				 exponent-value
				 exponent-length
				 type-of-float))

	(setf string-length (if end
				end
				(length string)))

	;; First, we attempt to parse an integer from the left-most part of the string
	;; storing the value and the length of the parsed integer in the respective
	;; values and incrementing our place in the full string
	;; This constitutes the whole part of the floating point number

	(multiple-value-setq (whole-part whole-length) (parse-int string :value-on-error nil))
	(incf length-marker whole-length) ;;increment relative position by length of first int

	;; If our previous operation didn't parse the entire string in one go,
	;; we increment our relative position in the string by 1 (within the subseq)
	;; in order to avoid the period in a floating point representation
	;; and parse a second integer from that point on.
	;; This consistutes the fractional part of the floating point number
	
	(if (not (eql string-length length-marker))
	    (progn
	      (multiple-value-setq (fraction-part fraction-length)
		(parse-int string :value-on-error nil :start (incf length-marker)))
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
		(parse-int string :value-on-error nil :start (incf length-marker)))))

	;; Now we build up the final float
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

(defun parse-complex-num (string &key (value-on-error nil)(start 0)(end nil))
  (handler-case (parse-number:parse-number
		 string
		 :start start
		 :end end
		 :float-format *default-float-format*)
    (error () value-on-error)))

(defun parse-string (string &key (value-on-error nil)(start 0)(end nil))
  "Return a deep copy of the input string"
  (if (stringp string)
      (let* ((string-end-index (if end
				   end
				   (- (length string) 1)))
	     (new-string (make-array (+ (- string-end-index start) 1) :element-type 'character)))
	(loop
	   for i from start to string-end-index
	   do (setf (schar new-string i) (aref string i)))
	new-string)
      value-on-error))
  
(defun parse-type-from-string (string &key (start 0)(end nil))
  "Obtain the most sensible type contained in a string"
  (let ((string-num-type (type-of (handler-case (parse-number:parse-number string :start start :end end)
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
  
(defun parse-number (string &key (value-on-error nil)(radix 10)(start 0)(end nil))
  (let ((type-of-string (parse-type-from-string string :start start :end end)))
    (cond ((eq type-of-string 'FLOAT)(parse-float string :value-on-error value-on-error :start start :end end))
	  ((eq type-of-string 'INTEGER)(parse-int string :value-on-error value-on-error :radix radix :start start :end end))
	  ((eq type-of-string 'RATIO)(parse-ratio string :value-on-error value-on-error :start start :end end))
	  ((eq type-of-string 'COMPLEX)(parse-complex-num string :value-on-error value-on-error :start start :end end))
	  ((eq type-of-string 'STRING)(parse-string string :value-on-error value-on-error :start start :end end))
	  (t value-on-error))))


