;;
;; recursive macro
;;   elegant but not as efficient as the next
;;

(macro let*
   (lambda (exp)
     (let ((vars (cadr exp))
	   (body (cddr exp)))
       (if (null? vars)
	   `(begin ,@body)
	   `(let (,(car vars)) (let* ,(cdr vars) ,@body))))))

;;
;; recurive auxiliary
;;   uses an auxiliary function to minimize reconstruction
;;

(macro let*
   (lambda (exp)
     (letrec ((xform
	       (lambda (vars body)
		 (if (null? vars)
		     `(begin ,@body)
		     `(let (,(car vars))
			,(xform (cdr vars) body))))))
       (let ((vars (cadr exp))
	     (body (cddr exp)))
	 (xform vars body)
	 ))))
