(define verbose #t)
(define enable-clambda #f)
(define enable-grs #f)
(define enable-cset #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                         C o m p i l e r
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (trace . x)
  (display x)
  (newline)
  (flush-output *standard-output*))

(define (closure-attributes proc)
  (list (%closure-code proc)
	(%closure-benv proc)
	(%closure-vars proc)
	(%closure-numv proc)
	(%closure-rest proc)))

(define ca closure-attributes)


(define (compile exp env)
  (if verbose (trace "compile:" exp env))
  (cond ((symbol? exp) 
	 (compile-symbol exp env))
	((atom? exp) 
	 exp)
	((pair? exp) 
	 (let ((x (car exp)))
	   (cond ((eq? x 'quote)  exp)
		 ((eq? x 'if)     (compile-if exp env))
		 ((eq? x 'cond)   (compile-cond exp env))
		 ((eq? x 'while)  (compile-while exp env))
		 ((eq? x 'lambda) (compile-lambda exp env))
		 ((eq? x 'set!)   (compile-set! exp env))
		 ((eq? x 'define) (compile-define (normalize-define exp) env))
		 ((eq? x 'let)    (compile-let exp env))
		 ((eq? x 'letrec) (compile-letrec exp env))
		 ((eq? x 'let*)   (compile (let*->let exp) env))
		 ((eq? x 'access) (compile-access exp env))
		 ((eq? x 'delay)  (compile-delay exp env))
		 ((or (eq? x 'and)
		      (eq? x 'or)
		      (eq? x 'begin)
		      (eq? x 'sequence)) 
		  (cons x (compile-list (cdr exp) env)))
		 (else 
		  (compile-application exp env)))))
	(else
	 (error "unrecognized expression" exp))
	))

;;
;; symbol
;;
(define (lookup-symbol sym env depth)
    (if (null? env)
	(%gref sym)
      (let ((bindings (environment-bindings env))
	    (index 0)
	    code)
	(while (and (not code) bindings)
	  (if (eq? sym (caar bindings))
	      (set! code (%fref depth index))
	    (begin
	     (set! index (1+ index))
	     (set! bindings (cdr bindings)))))
	(if (not code)
	    (lookup-symbol sym (environment-parent env) (1+ depth))
	  code))))

;;
;; symbol
;;
;;   <symbol> -> (%gref <symbol>)
;;   <symbol> -> (%fref <depth> <index>)
;;
(define (compile-symbol exp env)
  (if verbose (trace "compile-symbol:" exp env))
  (if enable-grs
      (lookup-symbol exp env 0)
    exp))

;;
;; if
;;
(define (compile-if exp env)
  (list 'if (compile (cadr exp) env) 
	(compile (caddr exp) env) (compile (cadddr exp) env)))

;;
;; cond
;;
(define (compile-cond exp env)
  ;; (cond <cond-clauses>)
  (if verbose (trace "cond:" exp env))
  (cons 'cond (compile-cond-clauses (cdr exp) env)))

(define (compile-cond-clauses exp env)
  ;; (<cond-clause> <cond-clause> ...)
  (if verbose (trace "cond-clauses:" exp env))
  (if (pair? exp)
      (cons (compile-cond-clause (car exp) env) (compile-cond-clauses (cdr exp) env))
    nil))

(define (compile-cond-clause exp env)
  ;; <test> <list> |
  ;; else <list>
  (if verbose (trace "cond-clause:" exp env))
  (if (eq? (car exp) 'else)
      (cons 'else (compile-list (cdr exp) env))
    (compile-list exp env)))

;;
;; while
;;
(define (compile-while exp env)
  (cons 'while (compile-list (cdr exp) env)))

;;
;; lambda
;;
;;    (lambda <args> <body>) --> (clambda <evaluated-lambda>)
;;
(define (compile-lambda exp env)
  (if verbose (trace "compile-lambda:" exp env))
  (let ((arg-list (cadr exp)))
    (let ((the-lambda (cons 'lambda
			    (cons arg-list
				  (compile-list (cddr exp) (extend-env arg-list env))))))
      (if enable-clambda
	  (list 'clambda (%prim-eval the-lambda env))
	the-lambda))))

;;
;; let
;;
;;     (let <bindings> <body>)
;;
(define (compile-let exp env)
  (if verbose (trace "compile-let" exp env))
  (let ((<bindings> (cadr exp)))
    (let ((<cbinds> (compile-let-bindings <bindings> env))
	  (<xenv> (extend-env (get-let-vars <bindings>) env)))
      (let ((<body> (compile-list (cddr exp) <xenv>)))
	(append '(let) (list <cbinds>) <body>)
	))))

;;
;; letrec
;;
;;     (letrec <bindings> <body>)
;;
(define (compile-letrec exp env)
  (if verbose (trace "compile-let" exp env))
  (let ((<bindings> (cadr exp)))
    (let ((<xenv> (extend-env (get-let-vars <bindings>) env)))
      (let ((<cbinds> (compile-let-bindings <bindings> <xenv>))
	    (<body> (compile-list (cddr exp) <xenv>)))
	(append '(letrec) (list <cbinds>) <body>)
	))))

(define (get-let-vars <bindings>)
  ;;(if verbose (trace "compile-let-vars" <bindings>))
  (if (null? <bindings>)
      nil
    (let ((x (car <bindings>)))
      (if (pair? x)
	  (cons (car x) (get-let-vars (cdr <bindings>)))
	(cons x (get-let-vars (cdr <bindings>)))))))

(define (compile-let-bindings <bindings> env)
  ;;(if verbose (trace "compile-let-bindings" <bindings>))
  (if (null? <bindings>)
      nil
    (let ((<bind> (car <bindings>)))
      (if (pair? <bind>)
	  (cons (list (car <bind>) (compile (cadr <bind>) env))
		(compile-let-bindings (cdr <bindings>) env))
	(cons <bind> (compile-let-bindings (cdr <bindings> env)))))))

;;
;; access
;;
(define (compile-access exp env)
  (let ((x (cadr exp)))
    (if (symbol? x)
	(list 'access x (compile (caddr exp) env))
      (error "access expects a symbol" exp))))

;;
;; delay
;;
(define (compile-delay exp env)
  (list 'delay (cadr exp)))
;;
;; set!
;;
(define (compile-set! exp env)
  ;; ultimately transform this into a lookup of the lexical env
  ;;   or an access expression
  (let ((x (cadr exp)))
    (cond ((symbol? x) 
	   (if enable-cset
	       (list 'cset! (lookup-symbol x env 0) (compile (caddr exp) env))
	     (list 'set! x (compile (caddr exp) env))))
	  ((and (pair? x) (eq? (car x) 'access)) 
	   (list 'set! (compile-access x env) (compile (caddr exp) env)))
	  (else
	   (error "illegal target for set!" x)))))

;;
;; define
;;
(define (compile-define exp env)
  (list 'define (cadr exp) (compile (caddr exp) env)))

;;
;; application
;;
(define (compile-application exp env)
  (compile-list exp env))

(define (compile-list exp env)
  (if (null? exp) 
      nil 
    (cons (compile (car exp) env) (compile-list (cdr exp) env))))

(define (normalize-arg-list x)
  (cond ((null? x) x)
	((symbol? x) (cons x nil))
	((and (pair? x) (symbol? (car x)))
	 (cons (car x) (normalize-arg-list (cdr x))))
	(else
	 (error "badly formed arg-list tail" x))))

(define (make-empty-bindings arg-list)
  (if (null? arg-list)
      ()
    (cons (cons (car arg-list) nil) (make-empty-bindings (cdr arg-list)))))

(define (extend-env arg-list env)
  (let ((x (normalize-arg-list arg-list)))
    (%make-environment (make-empty-bindings x) env)))

;;
;; transformer(s)
;;
;;    let*->let
;;
(define (let*->let exp)   (%transform-let* exp))

;;
;; Nested Defines
;;
;;   (define (foo a b)
;;      (define (mul x y) (* x y))
;;      (define (add x y) (+ x y))
;;      (mul a (add a b)) )
;;
;;    ==>
;;
;;   (define (foo a b)
;;     (let (mul add)
;;       (set! mul (lambda (x y) (* x y)))
;;       (set! add (lambda (x y) (+ x y)))
;;       (mul a (add a b)) ))
;;

(define (normalize-define d)
  (if verbose (trace "normalize-define" d))
  (if (not (and (pair? d) (eq? (car d) 'define)))
      (error "not a define" d)
    (if (eq? (car d) 'define)
	(let ((x (cadr d)))
	  (if (symbol? x)
	      d
	    (if (not (pair? d))
		(error "cannot normalze define" d)
	      (let ((sym (car x))
		    (args (cdr x))
		    (body (cddr d)))
		(list 'define sym (append '(lambda) (list args) body)))))))))

(define (accumulate-defines body)
  (if verbose (trace "accumulate-defines" body))
  ;;  gather a list of internal defines and non-defines, other s-expressions
  (let ((defines '())
	(sexprs '()))
    (while body
      (let ((x (car body)))
	(if (and (pair? x) (eq? (car x) 'define))
	    (set! defines (cons (normalize-define x) defines))
	  (set! sexprs (cons x sexprs)))
	(set! body (cdr body))))
    (cons defines sexprs)))

(define (makeset d)
  (cons 'set! (cdr d)))


(define (transform-nested-defines d)
  (if verbose (trace "transform-nd"))
  (let ((<nd> (normalize-define d)))
    (if (not (pair? (caddr <nd>)))
	<nd>
      (let ((<name> (cadr <nd>))
	    (<lambda> (caddr <nd>)))
	(let ((<body> (cddr <lambda>)))
	  (let ((pair (accumulate-defines <body>)))   ;; (<defines> . <sexprs>)
	    (if (null? (car pair))
		<nd>
	      (let ((<vars> (map cadr (car pair)))
		    (<sets> (map makeset (car pair)))
		    (<sexprs> (reverse (cdr pair)))
		    (<params> (cadr <lambda>)))
		`(define ,<name> (lambda ,<params> (let ,<vars> ,@<sets> ,@<sexprs>)))
		))))))))

(if #f (begin

(transform-nested-defines
 '(define (foo a b)
    (define bar a)
    (define bob b)
    (+ bar bob)
    ))

))

;; [EOF]
