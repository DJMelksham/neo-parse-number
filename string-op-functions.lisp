

(defun parse-kind-from-string (string)
  "A function that determines whether a string can be parsed as a
   string, an integer, or a float.  May be expanded/specialised in the future."
  (flet ((exponent-match (char exponent-state position)
	   (if exponent-state
	       (return-from exponent-match nil)
	       (case char 	   
		 ((#\e #\E #\d #\D #\f #\F #\s #\S #\l #\L)
		  position)))))
    (let ((char-digit-p-result nil)
	  (string-length (length string))
	  (sign-p nil)
	  (decimal-point-p nil)
	  (digits-p nil)
	  (exponent-p nil)
	  (exponent-sign-p nil)
	  (exponent-digits-p nil
	  (exponent-marker-p nil)
	  
      
      (loop
	 for char across string
	 for i = 0 then (incf i)
	 do (setf char-digit-p-result (digit-char-p char))

	 ;; On the first iteration, there are a number of checks that either need to be made
	 ;; or let us potentially short-circuit out of parsing further and return the string
	 ;; kind.  If the first character is not a digit, check/set if first character is a
	 ;; negation sign, if first char is not a
	 ;; negation sign, check 
	   
	   (if (and (eql i 0)
		    (not (char-digit-p-result)))
	       (if (char= char #\-)
		   (setf signal-negation t)
		   (return-from parse-kind-from-string 'string))
	       
	       ;; On the later iterations, or if it actually was a digit character
	       ;; on the first iteration, we check to see if the character was a digit
	       (if (and char-digit-p-result
			
		   (incf string-num-digits)
		   
		   ;; If it isn't, we do a bunch of other stuff...
		   (progn
		     ;; If its a period, we check if a period has already been seen
		     ;; If it has, then its a string, because none of the currently
		     ;; accepted number types can contain two periods.  If its one
		     ;; period, it could be a floating point number, so 
		     (if (char= char #\.)
			 (if signal-contains-period
			     (return-from parse-kind-from-string 'string)
			     (setf signal-contains-period i)))

		  
		  
		
		
			
    
    
    
    )
  
(defun string-to-thing (string)

  )

(defun string-to-type-of-thing (string type)

  )


(let ((float-format *read-default-float-format*))
  (defun change-float-format (new-format)

    (cond ((eq new-format 'single-float) (setf float-format 'single-float))
	  ((eq new-format 'double-float) (setf float-format 'double-float))
	  ((eq new-format 'short-float) (setf float-format 'short-float))
	  ((eq new-format 'long-float) (setf float-format 'long-float))
	  ((eq new-format 'single) (setf float-format 'single-float))
	  ((eq new-format 'double) (setf float-format 'double-float))
	  ((eq new-format 'short) (setf float-format 'short-float))
	  ((eq new-format 'long) (setf float-format 'long-float))
	  (t float-format)))

  (defun parse-float (string)
    
    )

  (defun parse-single (string)
    
    )

  (defun parse-double (string)
    
    )

  (defun parse-fraction (string)
    
    )

  (defun parse-complex (string)
    
    )
  )
