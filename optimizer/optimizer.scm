(define opt-verbose #f)
(define enable-clambda #f)
(define enable-grs #f)
(define enable-cset #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                        O p t i m i z e r
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


(define (optimize exp env)
  (if opt-verbose (trace "optimize:" exp env))
  (cond ((symbol? exp) 
	 (optimize-symbol exp env))
	((atom? exp) 
	 exp)
	((pair? exp) 
	 (let ((x (car exp)))
	   (cond ((eq? x 'quote)  exp)
		 ((eq? x 'if)     (optimize-if exp env))
		 ((eq? x 'cond)   (optimize-cond exp env))
		 ((eq? x 'while)  (optimize-while exp env))
		 ((eq? x 'lambda) (optimize-lambda exp env))
		 ((eq? x 'set!)   (optimize-set! exp env))
		 ((eq? x 'define) (optimize-define (normalize-define exp) env))
		 ((eq? x 'let)    (optimize-let exp env))
		 ((eq? x 'letrec) (optimize-letrec exp env))
		 ((eq? x 'let*)   (optimize (let*->let exp) env))
		 ((eq? x 'access) (optimize-access exp env))
		 ((eq? x 'delay)  (optimize-delay exp env))
		 ((or (eq? x 'and)
		      (eq? x 'or)
		      (eq? x 'begin)
		      (eq? x 'sequence)) 
		  (cons x (optimize-list (cdr exp) env)))
		 (else 
		  (optimize-application exp env)))))
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
(define (optimize-symbol exp env)
  (if opt-verbose (trace "optimize-symbol:" exp env))
  (if enable-grs
      (lookup-symbol exp env 0)
    exp))

;;
;; if
;;
(define (optimize-if exp env)
  (list 'if (optimize (cadr exp) env) 
	(optimize (caddr exp) env) (optimize (cadddr exp) env)))

;;
;; cond
;;
(define (optimize-cond exp env)
  ;; (cond <cond-clauses>)
  (if opt-verbose (trace "cond:" exp env))
  (cons 'cond (optimize-cond-clauses (cdr exp) env)))

(define (optimize-cond-clauses exp env)
  ;; (<cond-clause> <cond-clause> ...)
  (if opt-verbose (trace "cond-clauses:" exp env))
  (if (pair? exp)
      (cons (optimize-cond-clause (car exp) env) (optimize-cond-clauses (cdr exp) env))
    nil))

(define (optimize-cond-clause exp env)
  ;; <test> <list> |
  ;; else <list>
  (if opt-verbose (trace "cond-clause:" exp env))
  (if (eq? (car exp) 'else)
      (cons 'else (optimize-list (cdr exp) env))
    (optimize-list exp env)))

;;
;; while
;;
(define (optimize-while exp env)
  (cons 'while (optimize-list (cdr exp) env)))

;;
;; lambda
;;
;;    (lambda <args> <body>) --> (clambda <evaluated-lambda>)
;;
(define (optimize-lambda exp env)
  (if opt-verbose (trace "optimize-lambda:" exp env))
  (let ((arg-list (cadr exp)))
    (let ((the-lambda (cons 'lambda
			    (cons arg-list
				  (optimize-list (cddr exp) (extend-env arg-list env))))))
      (if enable-clambda
	  (list 'clambda (%prim-eval the-lambda env))
	the-lambda))))

;;
;; let
;;
;;     (let <bindings> <body>)
;;
(define (optimize-let exp env)
  (if opt-verbose (trace "optimize-let" exp env))
  (let ((<bindings> (cadr exp)))
    (let ((<cbinds> (optimize-let-bindings <bindings> env))
	  (<xenv> (extend-env (get-let-vars <bindings>) env)))
      (let ((<body> (optimize-list (cddr exp) <xenv>)))
	(append '(let) (list <cbinds>) <body>)
	))))

;;
;; letrec
;;
;;     (letrec <bindings> <body>)
;;
(define (optimize-letrec exp env)
  (if opt-verbose (trace "optimize-let" exp env))
  (let ((<bindings> (cadr exp)))
    (let ((<xenv> (extend-env (get-let-vars <bindings>) env)))
      (let ((<cbinds> (optimize-let-bindings <bindings> <xenv>))
	    (<body> (optimize-list (cddr exp) <xenv>)))
	(append '(letrec) (list <cbinds>) <body>)
	))))

(define (get-let-vars <bindings>)
  ;;(if opt-verbose (trace "optimize-let-vars" <bindings>))
  (if (null? <bindings>)
      nil
    (let ((x (car <bindings>)))
      (if (pair? x)
	  (cons (car x) (get-let-vars (cdr <bindings>)))
	(cons x (get-let-vars (cdr <bindings>)))))))

(define (optimize-let-bindings <bindings> env)
  (if opt-verbose (trace "optimize-let-bindings" <bindings>))
  (if (null? <bindings>)
      nil
    (let ((<bind> (car <bindings>)))
      (if (pair? <bind>)
	  (cons (list (car <bind>) (optimize (cadr <bind>) env))
		(optimize-let-bindings (cdr <bindings>) env))
	(cons <bind> (optimize-let-bindings (cdr <bindings>) env))))))

;;
;; access
;;
(define (optimize-access exp env)
  (let ((x (cadr exp)))
    (if (symbol? x)
	(list 'access x (optimize (caddr exp) env))
      (error "access expects a symbol" exp))))

;;
;; delay
;;
(define (optimize-delay exp env)
  (list 'delay (cadr exp)))
;;
;; set!
;;
(define (optimize-set! exp env)
  ;; ultimately transform this into a lookup of the lexical env
  ;;   or an access expression
  (let ((x (cadr exp)))
    (cond ((symbol? x) 
	   (if enable-cset
	       (list 'cset! (lookup-symbol x env 0) (optimize (caddr exp) env))
	     (list 'set! x (optimize (caddr exp) env))))
	  ((and (pair? x) (eq? (car x) 'access)) 
	   (list 'set! (optimize-access x env) (optimize (caddr exp) env)))
	  (else
	   (error "illegal target for set!" x)))))

;;
;; define
;;
(define (optimize-define exp env)
  (list 'define (cadr exp) (optimize (caddr exp) env)))

;;
;; application
;;
(define (optimize-application exp env)
  (optimize-list exp env))

(define (optimize-list exp env)
  (if (null? exp) 
      nil 
    (cons (optimize (car exp) env) (optimize-list (cdr exp) env))))

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
(define (let*->let exp)
  (letrec ((xform 
	    (lambda (vars body)
	      (if (null? vars)
		  (cons 'begin body)
		  (list 'let (list (car vars)) 
			  (xform (cdr vars) body))))))
    (let ((vars (cadr exp))
	  (body (cddr exp)))
      (xform vars body))))

(if #f
(begin
  (let*->let '(let*))
  (let*->let '(let* ()))
  (let*->let '(let* () 1))
  (let*->let '(let* ((a 1)) a))
  (let*->let '(let* ((a 1) (b a)) (cons a b)))
  (let*->let '(let* ((a 10) (b (* a 2)) (c (+ a b))) (list a b c)))
))

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
  (if opt-verbose (trace "normalize-define" d))
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
  (if opt-verbose (trace "accumulate-defines" body))
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
  (if opt-verbose (trace "transform-nd"))
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
