
 (defun ug ()
   (let ((arr "534959"))
     (declare (optimize (speed 3) (safety 1))
	      (type (simple-array character) arr))
     (loop repeat 1000000
	do (loop for char across arr
	      for length = (length arr) then (decf length)
	      with result = 0
	      do (incf result (* (expt 10 length) (digit-char-p char)))
	      finally (return result)))))
