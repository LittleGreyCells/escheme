;;; -*- Mode: Lisp -*-

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
	      (lambda (exp1)
		(let ((qq-car (qq (car exp1)))
		      (qq-cdr (qq (cdr exp1))))
		  (if (and (pair? qq-car) 
			   (eq? (car qq-car) append-me-sym))
		      (list 'append (cdr qq-car) qq-cdr)
		    (list 'cons qq-car qq-cdr)))))
	     (QQ
	      (lambda (exp2)
		(cond ((symbol? exp2)
		       (list 'quote exp2))
		      ((vector? exp2) 
		       (list 'list->vector (qq (vector->list exp2))))
		      ((atom? exp2) 
		       exp2)
		      ((eq? (car exp2) 'quasiquote)
		       (set! qq-lev (1+ qq-lev))
		       (let ((qq-val
			      (if (= qq-lev 1)
				  (qq (cadr exp2))
				(qq-car-cdr exp2))))
			 (set! qq-lev (-1+ qq-lev))
			 qq-val))
		      ((or (eq? (car exp2) 'unquote)
			   (eq? (car exp2) 'unquote-splicing))
		       (set! qq-lev (-1+ qq-lev))
		       (let ((qq-val
			      (if (= qq-lev 0)
				  (if (eq? (car exp2) 'unquote-splicing)
				      (cons append-me-sym 
					    (%expand-macros (cadr exp2)))
				    (%expand-macros (cadr exp2))) 
				(qq-car-cdr exp2))))
			 (set! qq-lev (1+ qq-lev))
			 qq-val))
		      (else
		       (qq-car-cdr exp2))))))
	    (let ((expansion (qq x)))
	      (if check-qq-expansion-flag
		  (check-qq-expansion expansion))
	      expansion))))

(define CHECK-QQ-EXPANSION
  (lambda (exp3)
    (cond ((vector? exp3)
	   (check-qq-expansion (vector->list exp3)))
	  ((atom? exp3)
	   #f)
	  (else
	   (if (eq? (car exp3) append-me-sym)
	       (error "UNQUOTE-SPLICING in unspliceable position"
		      (list 'unquote-splicing (cdr exp3)))
	     (or (check-qq-expansion (car exp3))
		 (check-qq-expansion (cdr exp3))))))))

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
