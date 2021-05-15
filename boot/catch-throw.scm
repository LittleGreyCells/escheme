;;
;; catch/throw
;;
;;   (catch <tag> <expr> ...)
;;   (throw <tag> <expr> ...)
;;
;;   Create a catch and throw pair of services to implement non-local exit.
;;
;;     Example of use:
;;
;;       (define exp1 (catch 'foo (let ((x 1)) x))) 
;;       (print exp)
;;       1
;;
;;       (define exp1 (catch 'foo (let ((x 1)) (throw 'foo (+ x 1)))))
;;       (print exp)
;;       2
;;
;;   How can we implement this using continuations?
;;
;;       (define exp1 (catch foo (let ((x 1)) (throw foo (+ x 1)))))
;;                               ^1
;;                                             ^2
;;                     ^3
;;         o (^1) somewhere after the first "foo" we need to save a continuation
;;           - the first return will allow us to process a create-catch for foo
;;
;;         o (^2) in the body, when the "throw foo" is encountered, we need to:
;;           - evaluate the expression "(+ x 1)"
;;           - fetch the continuation associated with foo
;;             = remove any intervening continuations
;;             = perform an all unwind-protect cleanup (see below)
;;           - apply the continuation
;;
;;         o (^3) the second return from the continuation
;;           - suppies the result of the last throw form
;;

;;
;; catch stack
;;
;;   This stack contains more that just catch continuations. It also contains unwind cleanup forms.
;;
;;     catch bundle  == (catch . (<tag> . <continuation>))
;;     unwind bundle == (unwind . (<env> . <cleanup-forms>))
;;

(define *catch-stack* nil)
(define (catch-stack) *catch-stack*)

(define (%push-catch <catch>)
  (set! *catch-stack* (cons <catch> *catch-stack*)))

(define (%pop-catch)
  (let ((top (car *catch-stack*)))
    (set! *catch-stack* (cdr *catch-stack*))
    top))

;;
;; catch/throw
;;

(define (%catch tag catch-forms env)
  (let ((ret (call/cc (lambda (cc) (cons 'catch (cons tag cc))))))
    (if (and (pair? ret) (eq? (car ret) 'catch))
	(begin
	  ;; (^1) push catch
	  ;; ret = ('catch . (tag . continuation))
	  (%push-catch ret)
	  (let (last)
	    (for-each (lambda (exp) (set! last (eval exp env))) catch-forms)
	    (%pop-catch)
	    last))
	  ;; (^3) process throw's return
	  ret
	  )))

(define (%throw tag throw-forms env)
  (let (target done)
    (while (not done)
       (if (null? (catch-stack))
	   (set! done #t)
	   (let ((x (%pop-catch)))
	     (cond ((eq? 'catch (car x))
		    ;; x = (catch . (<tag> . <continuation>))
		    (if (eq? tag (cadr x))
			(begin
			  (set! target (cddr x))
			  ;; (^2) target = <continuation>
			  (set! done #t))))
		   ((eq? 'unwind (car x))
		    ;; x = (unwind . (<env> . <cleanup-forms>))
		    (let ((env (cadr x))
			  (cleanup-forms (cddr x)))
		      (for-each (lambda (form) (eval form env)) cleanup-forms)))
		   (else
		    (error "bad non-local exit form" x))))))
    (if (null? target)
	(if (eq? tag 'error)
	    (error (eval (car throw-forms) env))
	    (error "no throw target found" tag)))
    (let (last)
      ;; (^2) evaluate throw forms
      (for-each (lambda (exp) (set! last (eval exp env))) throw-forms)
      ;; (^2) apply the continuation
      (target last)
      )))

(define (%unwind-all)
  (while (catch-stack)
     (let ((x (%pop-catch)))
       (if (eq? 'unwind (car x))
	   ;; x = (unwind . (<env> . <cleanup-forms>))
	   (let ((env (cadr x))
		 (cleanup-forms (cddr x)))
	     (for-each (lambda (form) (eval form env)) cleanup-forms))))))
       
(macro catch
  (lambda (exp)
    `(%catch ,(cadr exp) ',(cddr exp) (the-environment))))

(macro throw
  (lambda (exp)
    `(%throw ,(cadr exp) ',(cddr exp) (the-environment))))

(macro throw-error
  (lambda (exp)
    `(throw 'error ,(cadr exp))))
       
;;
;; unwind proctect
;;
;;   (unwind-protect <body-form> <cleanup-form> ...)
;;
;;   unwind-protect executes <body-form> with a guarantee that the <cleanup-forms> will be evaluated
;;   if control leaves body-form, no matter how that happens. body-form may complete normally,
;;   or execute a throw out of the unwind-protect, or cause an error; in all cases, the cleanup-forms
;;   will be evaluated.
;;
;;   If body-form finishes normally, unwind-protect returns the value of body-form, after it evaluates
;;   the cleanup-forms. If body-form does not finish, unwind-protect does not return any value in the
;;   normal sense.
;;
;;   Only body-form is protected by the unwind-protect. If any of the cleanup-forms themselves exits
;;   nonlocally (via a throw or an error), unwind-protect is not guaranteed to evaluate the rest of
;;   them. If the failure of one of the cleanup-forms has the potential to cause trouble, then protect
;;   it with another unwind-protect around that form.
;;
;;   The number of currently active unwind-protect forms counts, together with the number of local
;;   variable bindings, against the limit max-specpdl-size (see Local Variables).
;;

(define (%unwind-protect body-form cleanup-forms env)
  (let (ret)
    ;; the protected form
    (%push-catch (cons 'unwind (cons env cleanup-forms)))
    (set! ret (eval body-form env))
    (%pop-catch)
    ;; cleanup (Python finally)
    (for-each (lambda (exp) (eval exp env)) cleanup-forms)
    ret
    ))

(macro unwind-protect
  (lambda (exp)
    `(%unwind-protect ',(cadr exp) ',(cddr exp) (the-environment))))

;; [EOF]
