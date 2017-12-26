(define verbose #f)
(define enable-clambda #t)
(define enable-grs #t)
(define enable-cset #t)

(if #f
    (begin
     (load "compiler.scm")
     (load "./compiler/test_compiler.scm")
     (define x '(let ((a 1)) a))
     (define xab '(let ((a 1)(b 2)) b))
     (define x1 '(let ((a 1)) (+ a 1)))
     (define x2 '(let ((a 1)) (let ((b 2)) (+ a b))))
     (define x2e '(let ((a 1)) (let ((b 2)) (the-environment))))
     (define x3 '(letrec ((a 1) (b 2)) (+ a b)))
     (define cx (compile x '()))
     (define cxab (compile xab '()))
     (define cx1 (compile x1 '()))
     (define cx2 (compile x2 '()))
     (define cx2e (compile x2e '()))
     (define cx3 (compile x3 '()))
     (eval (compile x '()) )
     (eval (compile xab '()) )
     (eval (compile x1 '()) )
     (eval (compile x2 '()) )
     (eval (compile x2e '()) )
     (eval (compile x3 '()) )

     (define x '(lambda (n) n) '())
     (define y '(lambda (n) ((lambda (n) (* n n)) n)) '())
     (define z '(lambda (n) ((lambda (m) (* m n)) n)) '())
     (define cx (compile x '()))
     (define cy (compile y '()))
     (define cz (compile z '()))

     (define a 100)
     (define a1 '(set! x (* a 2)))
     (define ca1 (compile a1 '()))
     (define ca1g (compile 'x '()))

     (define frameset1 '(lambda (n) (set! n (* n 2)) n))
     (define frameset2 '(lambda (n) (let ((m (* n 2))) (set! n (+ n m)) n)))
     (define cframeset1 (compile frameset1 '()))
     (define cframeset2 (compile frameset2 '()))
     ((eval cframeset1) 10)
     ((eval cframeset2) 10)

     (set! a 10)
     (set! a 100)
     (set! a -200)
     (define if1 '(if (> a 0) a (- 0 a)))
     (define cif1 (compile if1 '()))
     (eval cif1)

     (define while1 '(while (> a 0) (print a) (set! a (- a 1))))
     (define cwhile1 (compile while1 '()))
     (eval cwhile1)

     (define e1 '(define env1 (let ((a 1) (b 2)) (the-environment))))
     (define ce1 (compile e1 '()))
     (eval ce1)

     (define acc1 '(access a env1))
     (define acc2 '(access b env1))
     (define cacc1 (compile acc1 '()))
     (define cacc2 (compile acc2 '()))
     (eval cacc1)
     (eval cacc2)

     ;; 
     ;; a performance study
     ;;

     (define g1 10)
     (define g2 20)

     ;; repeat
     (define define-repeat 
       '(define repeat 
	  (lambda (f n) 
	    (while (> n 0) 
	      (f) 
	      (set! n (-1+ n))))))

     ;; define foo
     (define define-foo 
       '(define foo 
	  (lambda (a b) 
	    (let ((x (+ a g1)) 
		  (y (* b g2))) 
	      (+ x y g1 g2)))))

     ;; define call foo
     (define define-call-foo 
       '(define call-foo 
	  (lambda () 
	    (foo 10 20))))

     ;; define repeat call foo
     (define define-repeat-call-foo 
       '(define repeat-call-foo 
	  (lambda () 
	    (repeat call-foo 10000))))

     ;; purely intepreted version

     (eval define-repeat)
     (eval define-foo)
     (eval define-call-foo)
     (eval define-repeat-call-foo)

     (time-it repeat-call-foo)

     (define (timed-test) (time-it repeat-call-foo))
     (perform-timed-run 10)

     ;; semi-compiled verion
    
     (define define-crepeat (compile define-repeat '()))
     (define define-cfoo (compile define-foo '()))
     (define define-ccall-foo (compile define-call-foo '()))
     (define define-repeat-ccall-foo (compile define-repeat-call-foo '()))

     (eval define-crepeat)
     (eval define-cfoo)
     (eval define-ccall-foo)
     (eval define-repeat-ccall-foo)

     ;; don't forget to compile the timing functions as well!
     (set! time-it (eval ( compile time-it '() )))
     (set! avg (eval ( compile avg '() )))
     (set! run-n-times (eval ( compile run-n-times '() )))
     (set! perform-timed-run (eval ( compile perform-timed-run '() )))

     (time-it repeat-call-foo)

     (define (timed-test) (time-it repeat-call-foo))
     (perform-timed-run 10)

     ;; uncompiled; no let xforms:    0.095103
     ;; only let xforms:              0.078303 (i.e. expand once, not every time)
     ;; only x+clambda:               0.071861
     ;; only x+gref/fref:             0.074800
     ;; only x+gref/fref/gset:        0.074593
     ;; add x+clambda/gref/fref/gset: 0.068685
     ))

(if #f (begin

(define foo 100)
(define a   200)
(define x '(let ((a 1) (b a)) (+ a b))) 
(define x '(letrec ((a 1) (b a)) (+ a b)))
(define x '(let ((a 1)) (let ((foo a)) (+ a foo))))
      

))

;;
;; Timing
;;

(if #f
    (begin
     '(define (foo a b)
       (define (mul x y) (* x y))
       (define (add x y) (+ x y))
       (mul a (add a b))
       )
     '(define bar (lambda (a b)
		    (define (mul x y) (* x y))
		    (define (add x y) (+ x y))
		    (mul a (add a b))
		   ))
     ))

(define (time-it f)
  (let ((start-time (gettime))
	end-time)
    (f)
    (set! end-time (gettime))
    (let ((secs (- (car end-time) (car start-time)))
	  (nsecs (- (cdr end-time) (cdr start-time)))
	  etime)
      (set! etime (+ (* secs 1000000000) nsecs))
      etime
    )))

(define (avg x)    
  (let ((n (length x))
	(s (apply + x)))
    (/ s (* 1.0 n))))

(define (run-n-times-aux f n results)
  (print n)
  (if (> n 0)
      (run-n-times-aux f (- n 1) (cons (f) results))
    results))

(define (run-n-times f n)
  (let ((results nil))
    (while (> n 0)
      (set! results (cons (f) results))
      (set! n (- n 1)))
    results
    ))

(if #f
(begin
(define (run-n-times f n)
  (display "run-n-times:") (print n)
  (run-n-times-aux f n nil))
))

(define (perform-timed-run n)
  (let ((factor 1000000000))
    (/ (avg (run-n-times timed-test n)) factor)))

