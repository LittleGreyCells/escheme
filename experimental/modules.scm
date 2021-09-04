;;
;; Escheme Module System
;;
;;   The escheme module system is an approximate implemenation of module
;;   system specified for stklos. Please see document STklosModuleSystem.txt
;;   for a fuller description of the service interfaces and semantics.
;;
;; Services
;;
;;   (module <name> <exp1> <exp2> ...)
;;   (export <sym1> <sym2> ...)
;;   (import <module-name1> <module-name2> ...)
;;
;;   (all-modules)                     returns <list-of-name-module-pairs>
;;   (find-module <module-name>)       returns <module>
;;   (current-module)                  returns <module>
;;   (module-name <module>)            returns <symbol>
;;   (module-imports <module>)         returns <list-of-module-names>
;;   (module-exports <module>)         returns <list-of-symbols>
;;   (module-symbols <module>)         returns <list-of-symbols>
;;   (imported-symbol? <sym> <module>) returns <boolean>
;;
;;   (symbol-value <sym> <module> [<default>])  returns <value>
;;   (symbol-value* <sym> <module> [<default>]) returns <value>
;;   (in-module <module-name> <sym-name>)       returns <value>
;;
;;   (select-module <module-name>)
;;

;;
;; Module Primitives
;;
(define (%make-module . env)
  (vector 'module (if env (car env)  (%make-assoc-env)) nil nil nil))

(define (%get-env m) (vector-ref m 1))
(define (%get-exports m) (vector-ref m 2))
(define (%get-imports m) (vector-ref m 3))
(define (%get-symbols m) (vector-ref m 4))
(define (%set-exports m x) (vector-set! m 2 x))
(define (%set-imports m x) (vector-set! m 3 x))
(define (%set-symbols m x) (vector-set! m 4 x))

(define (module? m)
  (and (vector? m) (eq? (vector-ref m 0) 'module)))

(define (in-module? s m)
  (let ((env (%get-env m)))
    (if (null? env)
	(bound? s)
	(%assoc-env-has? env s))))

;;
;; Module Services
;;
(define escheme (%make-module (the-global-environment)))

(define *all-modules* (list (cons 'escheme escheme)))
(define (all-modules) *all-modules*)

(define (%module-add <name>)
  (let ((nm (assoc <name> *all-modules*)))
    (if nm
	(cdr nm)
	(let ((m (%make-module)))
	  (set! *all-modules* (cons (cons <name> m) *all-modules*))
	  m))))

(define (find-module <name>)
  (let ((x (assoc <name> (all-modules))))
    (if x
	(cdr x)
	(error "module not found" <name>))))

(define *current-module* escheme)
(define (current-module) *current-module*)

(define (module-name <module>)
  (letrec ((loop
	    (lambda (<modules>)
	      (if (null? <modules>)
		  nil
		  (let ((x (car <modules>)))
		    (if (eq? (cdr x) <module>)
			(car x)
			(loop (cdr <modules>))))))))
    (loop (all-modules))))

(define (module-imports <module>)
  (let ((<imports> (map cdr (%get-imports <module>)))
	<counts>)
    (for-each
     (lambda (<import>)
       (let ((x (assoc <import> <counts>)))
	 (if x
	     (set-cdr! x (+ (cdr x) 1))
	     (set! <counts> (cons (cons <import> 1) <counts>)))))
     <imports>)
    (let (<modules>)
      (for-each
       (lambda (<count>)
	 (if (= (cdr <count>) (length (%get-exports (find-module (car <count>)))))
	     (set! <modules> (cons (car <count>) <modules>))))
       <counts>)
      <modules>)))

(define (%module-export <exports> <module>)
  (for-each
   (lambda (x)
     (let ((xps (%get-exports <module>)))
       (if (not (member x xps))
	 (%set-exports <module> (cons x xps)))))
   <exports>)
  nil)

(define (%module-import <imports> <module>)
  (for-each
   (lambda (<name>)
     (let ((nm (assoc <name> (all-modules))))
       (if (not nm)
	   (error "module not defined" <name>))
       (let ((<exports> (%get-exports (cdr nm))))
	 (for-each
	  (lambda (<item>)
	    (let ((imps (%get-imports <module>)))
	      (if (not (assoc <item> imps))
		  (begin
		    (eval `(define ,<item> (eval ',<item> (%get-env ,(cdr nm)))) (%get-env <module>))
		    (%set-imports <module> (cons (cons <item> <name>) imps))))))
	  <exports>)
	 )))
   <imports>)
  nil)

(macro export
  (lambda (x)
    `(%module-export ',(cdr x) (current-module))))

(macro import
  (lambda (x)
    `(%module-import ',(cdr x) (current-module))))

(define (module-exports <module>)
  (%get-exports <module>))

(define (module-symbols <module>)
  (%get-symbols <module>))

(define (imported-symbol? <sym> <module>)
  (member <sym> (map car (%get-imports <module>))))

(define (symbol-value <sym> <module> . default)
  (if (and (in-module? <sym> <module>)
	   (not (imported-symbol? <sym> <module>)))
      (eval <sym> (%get-env <module>))
      (if (null? default)
	  (error "symbol not in module" <sym> <module>)
	  (car default))))

(define (symbol-value* <sym> <module> . default)
  (if (in-module? <sym> <module>)
      (eval <sym> (%get-env <module>))
      (if (null? default)
	  (error "symbol not in module" <sym> <module>)
	  (car default))))

(macro in-module
  (lambda (x)
    `(symbol-value* ',(caddr x) (find-module ',(cadr x)))
    ))

(define (%module-build <name> <sexprs>)
  (let ((<module> (%module-add <name>)))
    (eval `(define (current-module) ,<module>) (%get-env <module>))
    (for-each
     (lambda (x)
       (eval x (%get-env <module>))
       (if (eq? (car x) 'define)
	   (let* ((x (cadr x))
		  (sym (if (symbol? x) x (car x)))
		  (symbols (%get-symbols <module>)))
	     (if (not (member sym symbols))
		 (%set-symbols <module> (cons sym symbols))))))
     <sexprs>)
    <module>
    ))

(macro module
  (lambda (x)
    (set! x (cdr x))
    (let ((name (car x)))
      `(define ,name (%module-build ',name ',(cdr x))))
    ))

;;
;; redefine rep loop
;;

(define (%make-prompt <module>)
  (string-append (symbol->string (module-name <module>)) "> "))

(macro select-module
  (lambda (x)
    `(begin
       (set! *current-module* (find-module ',(cadr x)))
       (set-prompt (%make-prompt *current-module*)))
    ))

(define (*rep-loop*)
  (set-prompt (%make-prompt (current-module)))
  (letrec ((loop
	    (lambda ()
	      (let ((sexpr (read *terminal*)))
		(add-history sexpr)
		(print (eval sexpr (%get-env (current-module)))))
	      (loop))))
    (loop)))

(*rep-loop*)

;;
;; [EOF]
;;

