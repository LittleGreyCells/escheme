;;
;; 09/18/2002  LMJ Modified for escheme
;;

;;; QQUOTE.S[CM] 01-14-89 11:34 AM by John Armstrong

;; Expands QUASIQUOTE/UNQUOTE/UNQUOTE according to Rev^3 Report specs.
;;
;; This file can be included as is in ESCHEME.SCM or can be incorporated 
;; into MACROS.SCM, with expander functions anywhere and macros after
;; definition of COMPILER-SYNTAX

(define APPEND-ME-SYM (gensym)) ;; must be a gensym to avoid capture in
				;; certain (pathological) situations

(define QQ-EXPANDER
  (lambda (x)
    (letrec ((qq-lev 0)
	     (QQ-CAR-CDR
	      (lambda (exp)
		(let ((qq-car (qq (car exp)))
		      (qq-cdr (qq (cdr exp))))
		  (if (and (pair? qq-car) 
			   (eq? (car qq-car) append-me-sym))
		      (list 'append (cdr qq-car) qq-cdr)
		    (list 'cons qq-car qq-cdr)))))
	     (QQ
	      (lambda (exp)
		(cond ((symbol? exp)
		       (list 'quote exp))
		      ((vector? exp) 
		       (list 'list->vector (qq (vector->list exp))))
		      ((atom? exp) 
		       exp)
		      ((eq? (car exp) 'quasiquote)
		       (set! qq-lev (1+ qq-lev))
		       (let ((qq-val
			      (if (= qq-lev 1)
				  (qq (cadr exp))
				(qq-car-cdr exp))))
			 (set! qq-lev (-1+ qq-lev))
			 qq-val))
		      ((or (eq? (car exp) 'unquote)
			   (eq? (car exp) 'unquote-splicing))
		       (set! qq-lev (-1+ qq-lev))
		       (let ((qq-val
			      (if (= qq-lev 0)
				  (if (eq? (car exp) 'unquote-splicing)
				      (cons append-me-sym 
					    (%expand-macros (cadr exp)))
				    (%expand-macros (cadr exp))) 
				(qq-car-cdr exp))))
			 (set! qq-lev (1+ qq-lev))
			 qq-val))
		      (else
		       (qq-car-cdr exp))))))
	    (let ((expansion (qq x)))
	      (if check-qq-expansion-flag
		  (check-qq-expansion expansion))
	      expansion))))

(define CHECK-QQ-EXPANSION
  (lambda (exp)
    (cond ((vector? exp)
	   (check-qq-expansion (vector->list exp)))
	  ((atom? exp)
	   #f)
	  (else
	   (if (eq? (car exp) append-me-sym)
	       (error "UNQUOTE-SPLICING in unspliceable position"
		      (list 'unquote-splicing (cdr exp)))
	     (or (check-qq-expansion (car exp))
		 (check-qq-expansion (cdr exp))))))))

(define CHECK-QQ-EXPANSION-FLAG #t)

(define UNQ-EXPANDER
  (lambda (x) (error "UNQUOTE outside QUASIQUOTE" x)))

(define UNQ-SPL-EXPANDER
  (lambda (x) (error "UNQUOTE SPLICING outside QUASIQUOTE" x)))

;; MACROS -- must be evaluated with MACRO system in place

(compiler-syntax QUASIQUOTE qq-expander)
(compiler-syntax UNQUOTE unq-expander)
(compiler-syntax UNQUOTE-SPLICING unq-spl-expander)

;; [EOF]
