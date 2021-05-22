;;
;; macro definitions
;;

(macro case
  (lambda (form)
    (let ((test (cadr form))
          (sym (gensym)))
      `(let ((,sym ,test))
         (cond ,@(map (lambda (x)
                        (cond ((eq? (car x) 'else)
                               x)
			      ((atom? (car x))
			       `((eqv? ,sym ',(car x)) ,@(cdr x)))
			      (else
                               `((memv ,sym ',(car x)) ,@(cdr x)))))
                      (cddr form)))))))

(macro do
  (lambda (exp)
    (let ((do-var-clause 
	   (lambda (exp)
	     (if (not (symbol? (car exp)))
		 (error "do var clause bad initial binding" exp))
	     (let ((<bind> (list (car exp) (cadr exp)))
		   (<step> (cddr exp)))
	       (if (null? <step>)
		   (cons <bind> nil)
		 (cons <bind> (list 'set! (car exp) (caddr exp))))))))
      (let ((<var-clauses> (map do-var-clause (cadr exp)))
	    (<test-clause> (caddr exp))
	    (<body> (cdddr exp)))
	;; assemble the parts
	(let ((<bindings> (map car <var-clauses>))
	      (<steps> (map cdr <var-clauses>))
	      (<test> (car <test-clause>))
	      (<results> (cdr <test-clause>)))
	  `(let ,<bindings>
	     (while (not ,<test>)
	       ,@<body>
	       ,@<steps>)
	     ,@<results>)
	  )))))

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

;; [EOF]
