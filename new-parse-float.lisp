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


(defun parse-int (string &optional (value-on-error nil) (radix 10)(start 0)(end nil))
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


