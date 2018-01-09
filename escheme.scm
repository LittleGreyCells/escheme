;;; -*- Mode: Lisp -*-
(define *version* "v1.0")

(load "macros.scm")
(load "qquote.scm")

;;
;; This file contains implementations of standard functions
;; which may migrate into escheme as primitives for speed.
;;

(define (error msg . object)
  (display "error: ")
  (display msg)
  (if object
      (begin (display " [") 
             (display object)
             (display "]")))
  (newline)
  (*toplevel*))

(define (fatal msg . object)
  (display "fatal error: ")
  (display msg)
  (if object
      (begin (display " [") 
             (display object)
             (display "]")))
  (newline)
  (exit))

;; cdr past the 1st n elements and return car of remaining list
(define (list-ref list n)
  (car (list-tail list n)))

;; arrays

(define (make-array d . r)
  ((lambda (v) 
     (if r 
	 ((lambda (i) 
	    (while (< i d) 
	      (vector-set! v i (apply make-array r)) 
	      (set! i (+ i 1)))) 0) ) v)
   (make-vector d)) )

(define (array-ref a i . r)
  ((lambda (e) 
     (if r 
	 (apply array-ref (list* e r)) e)) 
   (vector-ref a i)) )

(define (array-set! a i v . r)
  (if r
      (apply array-set! (list* (vector-ref a i) v r))
      (vector-set! a i v)))

;;
;; special forms macros
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

;; [EOF]
