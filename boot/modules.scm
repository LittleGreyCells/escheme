(define *all-modules* nil)

(define (all-modules) *all-modules*)

(define (%module-add <name>)
  (let ((nm (assoc <name> *all-modules*)))
    (if nm
	(cdr nm)
	(let ((m (%make-module)))
	  (set! *all-modules* (cons (cons <name> m) *all-modules*))
	  m))))

(define (%module-build <name> <sexprs>)
  (let ((<module> (%module-add <name>)))
    (for-each
     (lambda (n) (eval n <module>))
     <sexprs>)
    <module>
    ))

(define (module-name <module>)
  (letrec ((module-name
	    (lambda (<mods>)
	      (if (null? <mods>)
		  nil
		  (let ((nm (car <mods>)))
		    (if (eq? <module> (cdr nm))
			(car nm)
			(module-name (cdr <mods>))))))))
    (module-name (all-modules))))

(define (find-module <name>)
  (letrec ((find-module
	    (lambda (<modules>)
	      (if (null? <modules>)
		  nil
		  (let ((nm (car <modules>)))
		    (if (eq? <name> (car nm))
			(cdr nm)
			(find-module (cdr <modules>))))))))
    (find-module (all-modules))))

(macro module
  (lambda (exp)
    (set! exp (cdr exp))
    (let ((name (car exp)))
      `(%module-build ',name ',(cdr exp))
      )))

;; [EOF]
