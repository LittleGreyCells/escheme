;;
;; The following code is adapted from xscheme macros.s
;;
;;   10/18/2013  LMJ Original adaptation for escheme
;;   05/21/2021  LMJ Added support for internal defines
;;

;; modified for escheme
(define %%eval eval)
;; end

(define (%expand-macros expr)
  (if (pair? expr)
    (if (symbol? (car expr))
      (let ((expander (get (car expr) '%syntax)))
        (if expander
          (expander expr)
          (let ((expander (get (car expr) '%macro)))
            (if expander
              (%expand-macros (expander expr))
              (cons (car expr) (%expand-list (cdr expr)))))))
      (%expand-list expr))
    expr))

(define (%expand-list lyst)
  (if (pair? lyst)
    (cons (%expand-macros (car lyst)) (%expand-list (cdr lyst)))
    lyst))

;; modified for escheme
(define (eval expr . env)
  (if (null? env)
    (%%eval (%expand-macros expr))
    (%%eval (%expand-macros expr) (car env))))
;; end

(put 'macro '%macro
  (lambda (form)
    (list 'put
          (list 'quote (cadr form))
          (list 'quote '%macro)
          (caddr form))))

(macro syntax
  (lambda (form)
    #f))

(macro compiler-syntax
  (lambda (form)
    (list 'put
          (list 'quote (cadr form))
          (list 'quote '%syntax)
          (caddr form))))

(compiler-syntax quote
  (lambda (form) form))
	  
(compiler-syntax quasiquote
  (lambda (x)
    (qq-process (cadr x))))
    
;; modified for escheme
(define (%internal-definitions body)
  (letrec ((normalize
	    (lambda (d)
	      (let ((x (cadr d)))
		(if (symbol? x)
		    d
		    `(define ,(car x) (lambda ,(cdr x) ,@(cddr d))) ))))
	   (loop
	    (lambda (body bindings)
	      (if (and body (pair? (car body)) (eq? (caar body) 'define))
		  (let ((x (normalize (car body))))
		    (loop (cdr body) (cons `(,(cadr x) ,(caddr x)) bindings)))
		  (if bindings
		      `((letrec ,(reverse bindings) ,@body))
		      body)))))
    (loop body nil)))
;; end

(compiler-syntax lambda
  (lambda (form)
    (cons
      'lambda
      (cons
        (cadr form)
        (%expand-list (%internal-definitions (cddr form)))))))

(compiler-syntax define
  (lambda (form)
    (cons
      'define
      (cons
        (cadr form)
        (%expand-list (%internal-definitions (cddr form)))))))
  
(compiler-syntax set!
  (lambda (form)
    (cons
      'set!
      (cons
        (cadr form)
        (%expand-list (cddr form))))))

(define (%cond-expander lyst)
  (cond
      ((pair? lyst)
       (cons
         (if (pair? (car lyst))
           (%expand-list (car lyst))
           (car lyst))
         (%cond-expander (cdr lyst))))
      (else lyst)))

(compiler-syntax cond
  (lambda (form)
    (cons 'cond (%cond-expander (cdr form)))))

; The following code for expanding let/let*/letrec was donated by:
;
; Harald Hanche-Olsen
; The University of Trondheim
; The Norwegian Institute of Technology
; Division of Mathematics
; N-7034 Trondheim NTH
; Norway

(define (%expand-let-assignment pair)
  (if (pair? pair)
    (cons
      (car pair)
      (%expand-macros (cdr pair)))
    pair))

(define (%expand-let-form form)
  (cons
    (car form)
    (cons
      (let ((lyst (cadr form)))
        (if (pair? lyst)
          (map %expand-let-assignment lyst)
          lyst))
      (%expand-list (%internal-definitions (cddr form))))))

(compiler-syntax let %expand-let-form)
;; (compiler-syntax let* %expand-let-form)
(compiler-syntax letrec %expand-let-form)

(macro define-integrable
  (lambda (form)
    (cons 'define (cdr form))))

(macro declare
  (lambda (form) #f))

;; [END]
